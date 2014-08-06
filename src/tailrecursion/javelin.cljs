;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin
  (:require [tailrecursion.priority-map :refer [priority-map]]))

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bf-seq
  "Like tree-seq but traversal is breadth-first instead of depth-first."
  [branch? children root]
  (letfn [(walk [queue]
            (when-let [node (peek queue)]
              (lazy-seq
               (cons node (walk (into (pop queue)
                                      (if (branch? node) (children node))))))))]
    (walk (conj cljs.core.PersistentQueue.EMPTY root))))

(defn- safe-nth [coll i] (try (nth coll i) (catch js/Error _)))

;; javelin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare Cell cell? cell)

(def ^:dynamic *sync* nil)

(def  last-rank     (atom 0))
(defn next-rank [ ] (swap! last-rank inc))
(defn deref*    [x] (if (cell? x) @x x))

(defn propagate! [pri-map]
  (loop [queue pri-map]
    (when-let [next (first (peek queue))]
      (let [value     ((.-thunk next))
            continue? (not= value (.-prev next))
            reducer   #(assoc %1 %2 (.-rank %2))
            siblings  (pop queue)
            children  (.-sinks next)]
        (if continue? (set! (.-prev next) value))
        (recur (if continue? (reduce reducer siblings children) siblings))))))

(defn destroy-cell! [this]
  (let [srcs (.-sources this)]
    (set! (.-sources this) [])
    (set! (.-watches this) {})
    (doseq [src srcs]
      (when (cell? src)
        (set! (.-sinks src) (disj (.-sinks src) this))))))

(defn set-formula! [this & [f sources]]
  (destroy-cell! this)
  (set! (.-sources this) (if f (conj (vec sources) f) (vec sources)))
  (doseq [source (.-sources this)]
    (when (cell? source)
      (set! (.-sinks source) (conj (.-sinks source) this))
      (if (> (.-rank source) (.-rank this))
        (doseq [dep (bf-seq identity #(.-sinks %) source)]
          (set! (.-rank dep) (next-rank))))))
  (let [compute   #(apply (deref* (peek %)) (map deref* (pop %)))
        thunk     #(let [x (.-state this), y (compute (.-sources this))]
                     (doseq [[k f] (.-watches this)]
                       (when-not (= k ::cell) (f k this x y)))
                     (set! (.-state this) y))
        err-mesg  "formula cell can't be updated via swap! or reset!"]
    (-add-watch this ::cell
      (if f #(throw (js/Error. err-mesg)) #(propagate! (priority-map %2 (.-rank %2)))))
    (set! (.-input? this) (if f false true))
    (set! (.-thunk this) (if f thunk #(deref this)))
    (doto this (-> (priority-map (.-rank this)) propagate!))))

(deftype Cell [meta state rank prev sources sinks thunk watches input?]
  cljs.core/IPrintWithWriter
  (-pr-writer [this writer opts]
    (write-all writer "#<Cell: " (pr-str state) ">"))

  cljs.core/IMeta
  (-meta [this] meta)

  cljs.core/IDeref
  (-deref [this] (.-state this))

  cljs.core/IReset
  (-reset! [this new-value]
    (when *sync* (swap! *sync* assoc this rank))
    (let [old-value (.-state this)]
      (set! (.-state this) new-value)
      (when-not (nil? (.-watches this))
        (-notify-watches this old-value new-value))
      new-value))

  cljs.core/ISwap
  (-swap! [this f] (reset! this (f (.-state this))))
  (-swap! [this f a] (reset! this (f (.-state this) a)))
  (-swap! [this f a b] (reset! this (f (.-state this) a b)))
  (-swap! [this f a b xs] (reset! this (apply f (.-state this) a b xs)))

  cljs.core/IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (when-not (and *sync* (= key ::cell))
        (f key this oldval newval))))
  (-add-watch [this key f]
    (set! (.-watches this) (assoc watches key f)))
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key))))

(defn formula   [f]   (fn [& sources] (set-formula! (cell ::none) f sources)))
(def ^:deprecated lift formula)

(defn cell      [x]   (set-formula! (Cell. {} x (next-rank) x [] #{} nil {} nil)))
(defn cell?     [c]   (when (= (type c) Cell) c))
(defn input?    [c]   (when (and (cell? c) (.-input? c)) c))
(defn set-cell! [c x] (set! (.-state c) x) (set-formula! c))

(defn dosync* [thunk]
  (if *sync*
    (thunk)
    (binding [*sync* (atom (priority-map))]
      (thunk)
      (propagate! @*sync*))))

(defn alts! [& cells]
  (let [olds    (atom (repeat (count cells) ::none)) 
        tag-neq #(vector (not= %1 %2) %2)
        diff    #(->> %2 (map tag-neq %1) (filter first) (map second) distinct)
        proc    #(let [news (diff (deref olds) %&)] (reset! olds %&) news)]
    (apply (formula proc) cells))) 

(defn cell-map [f c]
  (let [cseq ((formula seq) c)]
    (map #((formula (comp f safe-nth)) cseq %) (range 0 (count @cseq)))))

(defn cell-doseq* [items f]
  (let [pool-size (cell 0)
        items-seq ((formula seq) items)
        cur-count ((formula count) items-seq)
        ith-item  #((formula safe-nth) items-seq %)]
    ((formula (fn [pool-size cur-count f ith-item reset-pool-size!]
                (when (< pool-size cur-count)
                  (doseq [i (range pool-size cur-count)] (f (ith-item i)))
                  (reset-pool-size! cur-count))))
     pool-size cur-count f ith-item (partial reset! pool-size))))
