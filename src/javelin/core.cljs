;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.core
  (:require-macros [javelin.core])
  (:require [tailrecursion.priority-map :refer [priority-map]]))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare Cell cell? cell input? lens?)

(def ^:private ^:dynamic *tx* nil)
(def ^:private last-rank (atom 0))

(defn- bf-seq
  "Like tree-seq but traversal is breadth-first instead of depth-first."
  [branch? children root]
  (letfn [(walk [queue]
            (when-let [node (peek queue)]
              (->> (when (branch? node) (children node))
                (into (pop queue)) walk (cons node) lazy-seq)))]
    (walk (conj cljs.core.PersistentQueue.EMPTY root))))

(defn- propagate* [pri-map]
  (when-let [next (first (peek pri-map))]
    (let [popq  (pop pri-map)
          old   (.-prev next)
          new   (if-let [f (.-thunk next)] (f) (.-state next))
          diff? (not= new old)]
      (when diff? (set! (.-prev next) new) (-notify-watches next old new))
      (recur (if-not diff? popq (reduce #(assoc %1 %2 (.-rank %2)) popq (.-sinks next)))))))

(defn  deref*     [x]   (if (cell? x) @x x))
(defn- next-rank  [ ]   (swap! last-rank inc))
(defn- cell->pm   [c]   (priority-map c (.-rank c)))
(defn- add-sync!  [c]   (swap! *tx* assoc c (.-rank c)))
(defn- safe-nth   [c i] (try (nth c i) (catch js/Error _)))
(defn- propagate! [c]   (if *tx* (doto c add-sync!) (doto c (-> cell->pm propagate*))))

;; javelin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn destroy-cell! [this & [keep-watches?]]
  (let [srcs (.-sources this)]
    (set! (.-sources this) [])
    (set! (.-update this) nil)
    (set! (.-thunk this) nil)
    (when-not keep-watches? (set! (.-watches this) {}))
    (doseq [src srcs]
      (when (cell? src)
        (set! (.-sinks src) (disj (.-sinks src) this))))))

(defn set-formula! [this & [f sources]]
  (destroy-cell! this true)
  (when f
    (set! (.-sources this) (conj (vec sources) f))
    (doseq [source (.-sources this)]
      (when (cell? source)
        (set! (.-sinks source) (conj (.-sinks source) this))
        (if (> (.-rank source) (.-rank this))
          (doseq [dep (bf-seq identity #(.-sinks %) source)]
            (set! (.-rank dep) (next-rank))))))
    (let [compute #(apply (deref* (peek %)) (map deref* (pop %)))]
      (set! (.-thunk this) #(set! (.-state this) (compute (.-sources this))))))
  (propagate! this))

(deftype Cell [meta state rank prev sources sinks thunk watches update]
  cljs.core/IPrintWithWriter
  (-pr-writer [this w _] (write-all w "#<Cell: " (pr-str state) ">"))

  cljs.core/IWithMeta
  (-with-meta  [this meta]  (Cell. meta state rank prev sources sinks thunk watches update))

  cljs.core/IMeta
  (-meta [this] meta)

  cljs.core/IDeref
  (-deref [this] (.-state this))

  cljs.core/IReset
  (-reset! [this x]
    (cond (lens? this)  ((.-update this) x)
          (input? this) (do (set! (.-state this) x) (propagate! this))
          :else         (throw (js/Error. "can't swap! or reset! formula cell")))
    (.-state this))

  cljs.core/ISwap
  (-swap! [this f]        (reset! this (f (.-state this))))
  (-swap! [this f a]      (reset! this (f (.-state this) a)))
  (-swap! [this f a b]    (reset! this (f (.-state this) a b)))
  (-swap! [this f a b xs] (reset! this (apply f (.-state this) a b xs)))

  cljs.core/IWatchable
  (-notify-watches [this o n] (doseq [[key f] watches] (f key this o n)))
  (-add-watch      [this k f] (set! (.-watches this) (assoc watches k f)))
  (-remove-watch   [this k]   (set! (.-watches this) (dissoc watches k))))

(defn cell?     [c]   (when (= (type c) Cell) c))
(defn formula?  [c]   (when (and (cell? c) (.-thunk c)) c))
(defn lens?     [c]   (when (and (cell? c) (.-update c)) c))
(defn input?    [c]   (when (and (cell? c) (not (formula? c))) c))
(defn set-cell! [c x] (set! (.-state c) x) (set-formula! c))
(defn formula   [f]   (fn [& sources] (set-formula! (cell ::none) f sources)))
(defn lens      [c f] (let [c ((formula identity) c)] (set! (.-update c) f) c))
(defn cell
  ([x] (set-formula! (Cell. nil x (next-rank) x [] #{} nil {} nil)))
  ([x & {:keys [meta]}] (set-formula! (Cell. meta x (next-rank) x [] #{} nil {} nil))))

(def ^:deprecated lift formula)

;; javelin util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dosync* [thunk]
  (let [bind #(binding [*tx* (atom (priority-map))] (%))
        prop #(let [tx @*tx*] (binding [*tx* nil] (propagate* tx)))]
    (if *tx* (thunk) (bind #(do (thunk) (prop))))))

(defn alts! [& cells]
  (let [olds    (atom (repeat (count cells) ::none))
        tag-neq #(vector (not= %1 %2) %2)
        diff    #(->> %2 (map tag-neq %1) (filter first) (map second) distinct)
        proc    #(let [news (diff (deref olds) %&)] (reset! olds %&) news)]
    (apply (formula proc) cells)))

(defn cell-map [f c]
  (let [cseq ((formula seq) c)]
    (map #((formula (comp f safe-nth)) cseq %) (range 0 (count @cseq)))))

(defn cell-doseq* [items-seq f]
  (let [pool-size (cell 0)
        cur-count ((formula count) items-seq)
        ith-item  #((formula safe-nth) items-seq %)]
    ((formula (fn [pool-size cur-count f ith-item reset-pool-size!]
                (when (< pool-size cur-count)
                  (doseq [i (range pool-size cur-count)] (f (ith-item i)))
                  (reset-pool-size! cur-count))))
     pool-size cur-count f ith-item (partial reset! pool-size))))
