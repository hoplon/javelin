;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.core
  #?(:cljs (:require-macros [javelin.macros])
     :clj  (:require javelin.macros))
  #?(:clj  (:refer-clojure :exclude [dosync]))
  #?(:cljs (:require [tailrecursion.priority-map :refer [priority-map]])
     :clj  (:require [clojure.data.priority-map :refer [priority-map]]))
  #?(:cljs (:require-macros [javelin.impl-macros :as x])
     :clj  (:require [javelin.impl-macros :as x]))
  #?(:clj  (:require [clojure.core :as c])))

;; NOTE: Below hack to mirror macros in this namespace when we're on Clojure
;; doesn't properly fix up line numbers, docstring, etc. Need a
;; better strategy.
#?(:clj (doseq [[sym var] (ns-publics 'javelin.macros)
                :when (:macro (meta var))
                :let [new-var (intern *ns* sym @var)]]
          (alter-meta! new-var assoc :macro true)))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare #?(:cljs Cell) cell? cell input? lens?)

(def ^:private ^:dynamic *tx* nil)
(def ^:private last-rank (atom 0))

(defn- bf-seq
  "Like tree-seq but traversal is breadth-first instead of depth-first."
  [branch? children root]
  (letfn [(walk [queue]
            (when-let [node (peek queue)]
              (->> (when (branch? node) (children node))
                (into (pop queue)) walk (cons node) lazy-seq)))]
    (walk (conj #?(:cljs cljs.core.PersistentQueue.EMPTY
                   :clj clojure.lang.PersistentQueue/EMPTY)
                root))))

#?(:clj
   (defprotocol IWatchable
     (-notify-watches [this o n])
     (-add-watch      [this k f])
     (-remove-watch   [this k])))

(defn- propagate* [pri-map]
  (when-let [next (first (peek pri-map))]
    (let [popq  (pop pri-map)
          old   (x/get (.-prev next))
          new   (if-let [f (x/get (.-thunk next))] (f) (x/get (.-state next)))
          diff? (not= new old)]
      (when diff? (x/dosync (x/set! (.-prev next) new)) (-notify-watches next old new))
      (recur (if-not diff? popq (reduce #(assoc %1 %2 (x/get (.-rank %2))) popq (x/get (.-sinks next))))))))

(defn  deref*     [x]   (if (cell? x) @x x))
(defn- next-rank  [ ]   (swap! last-rank inc))
(defn- cell->pm   [c]   (priority-map c (x/get (.-rank c))))
(defn- add-sync!  [c]   (swap! *tx* assoc c (x/get (.-rank c))))
(defn- safe-nth   [c i] (try (nth c i) (catch #?(:cljs js/Error :clj Exception) _)))
(defn- propagate! [c]   (if *tx* (doto c add-sync!) (doto c (-> cell->pm propagate*))))

;; javelin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn destroy-cell! [this & [keep-watches?]]
  (let [srcs (x/get (.-sources this))]
    (x/dosync
     (x/set! (.-sources this) [])
     (x/set! (.-update this) nil)
     (x/set! (.-thunk this) nil))
    (when-not keep-watches? (x/dosync (x/set! (.-watches this) {})))
    (doseq [src srcs]
      (when (cell? src)
        (x/dosync
         (x/set! (.-sinks src) (disj (x/get (.-sinks src)) this)))))))

(defn set-formula! [this & [f sources]]
  (destroy-cell! this true)
  (when f
    (x/dosync
     (x/set! (.-sources this) (conj (vec sources) f)))
    (doseq [source (x/get (.-sources this))]
      (when (cell? source)
        (x/dosync
         (x/set! (.-sinks source) (conj (x/get (.-sinks source)) this)))
        (if (> (x/get (.-rank source)) (x/get (.-rank this)))
          (doseq [dep (bf-seq identity #(x/get (.-sinks %)) source)]
            (x/dosync
             (x/set! (.-rank dep) (next-rank)))))))
    (let [compute #(apply (deref* (peek %)) (map deref* (pop %)))]
      (x/dosync
       (x/set! (.-thunk this)
               #(let [computed (compute (x/get (.-sources this)))]
                  (x/dosync (x/set! (.-state this) computed)))))))
  (propagate! this))

#?(:cljs
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
   :clj
   (deftype Cell [meta state rank prev sources sinks thunk watches update]
     clojure.lang.IObj
     (withMeta [this meta] (Cell. meta state rank prev sources sinks thunk watches update))
     (meta [this] @meta)

     clojure.lang.IDeref
     (deref [this] @(.-state this))

     clojure.lang.IAtom
     (reset [this x]
       (cond (lens? this)  (@(.-update this) x)
             (input? this) (do (c/dosync (ref-set (.-state this) x)) (propagate! this))
             :else         (throw (ex-info "can't swap! or reset! formula cell" {:cell this})))
       @(.-state this))
     (swap [this f]        (reset! this (f @(.-state this))))
     (swap [this f a]      (reset! this (f @(.-state this) a)))
     (swap [this f a b]    (reset! this (f @(.-state this) a b)))
     (swap [this f a b xs] (reset! this (apply f @(.-state this) a b xs)))

     IWatchable
     (-notify-watches [this o n] (doseq [[key f] @watches] (f key this o n)))
     (-add-watch      [this k f] (c/dosync
                                  (alter (.-watches this) assoc k f)))
     (-remove-watch   [this k]   (c/dosync
                                  (alter (.-watches this) dissoc watches k)))))

(defn cell?     [c]   (when (= (type c) javelin.core.Cell) c))
(defn formula?  [c]   (when (and (cell? c) (x/get (.-thunk c))) c))
(defn lens?     [c]   (when (and (cell? c) (x/get (.-update c))) c))
(defn input?    [c]   (when (and (cell? c) (not (formula? c))) c))
(defn set-cell! [c x] (x/dosync (x/set! (.-state c) x)) (set-formula! c))
(defn formula   [f]   (fn [& sources] (set-formula! (cell ::none) f sources)))
(defn lens      [c f] (let [c ((formula identity) c)] (x/dosync (x/set! (.-update c) f)) c))

#?(:cljs
   (defn cell
     ([x] (set-formula! (Cell. nil x (next-rank) x [] #{} nil {} nil)))
     ([x & {:keys [meta]}] (set-formula! (Cell. meta x (next-rank) x [] #{} nil {} nil))))
   :clj
   (defn cell
     ([x] (set-formula! (Cell. (ref nil) (ref x) (ref (next-rank)) (ref x) (ref []) (ref  #{}) (ref nil) (ref {}) (ref nil))))
     ([x & {:keys [meta]}] (set-formula! (Cell. (ref meta) (ref x) (ref (next-rank)) (ref x) (ref []) (ref #{}) (ref nil) (ref {}) (ref nil))))))

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
