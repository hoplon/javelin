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

;; javelin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare Cell cell? cell)

(def  last-rank     (atom 0))
(defn next-rank [ ] (swap! last-rank inc))
(defn deref*    [x] (if (cell? x) @x x))

(defn propagate! [cell]
  (loop [queue (priority-map cell (.-rank cell))]
    (when (seq queue)
      (let [next      (key (peek queue))
            value     ((.-thunk next))
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
    (doseq [src (filter cell? srcs)]
      (set! (.-sinks src) (disj (.-sinks src) this)))))

(defn set-formula! [this & [f sources]]
  (destroy-cell! this)
  (set! (.-sources this) (if f (conj (vec sources) f) (vec sources)))
  (doseq [source (filter cell? (.-sources this))]
    (set! (.-sinks source) (conj (.-sinks source) this))
    (if (> (.-rank source) (.-rank this))
      (doseq [dep (bf-seq identity #(.-sinks %) source)]
        (set! (.-rank dep) (next-rank)))))
  (let [compute   #(apply (deref* (peek %)) (map deref* (pop %)))
        thunk     #(let [x (.-state this), y (compute (.-sources this))]
                     (doseq [[k f] (dissoc (.-watches this) ::cell)] (f k this x y))
                     (set! (.-state this) y))
        err-mesg  "formula cell can't be updated via swap! or reset!"]
    (-add-watch this ::cell (if f #(throw (js/Error. err-mesg)) #(propagate! %2)))
    (set! (.-input? this) (if f false true))
    (set! (.-thunk this) (if f thunk #(deref this)))
    (doto this propagate!)))

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
    (doseq [[key f] watches] (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.-watches this) (assoc watches key f)))
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key))))

(defn lift      [f]   (fn [& sources] (set-formula! (cell ::none) f sources)))

(defn cell      [x]   (set-formula! (Cell. {} x (next-rank) x [] #{} nil {} nil)))
(defn cell?     [c]   (when (= (type c) Cell) c))
(defn input?    [c]   (when (and (cell? c) (.-input? c)) c))
(defn set-cell! [c x] (set! (.-state c) x) (set-formula! c))

(defn alts! [& cells]
  (let [olds    (atom (repeat (count cells) ::none)) 
        tag-neq #(vector (not= %1 %2) %2)
        diff    #(->> %2 (map tag-neq %1) (filter first) (map second) distinct)
        proc    #(let [news (diff (deref olds) %&)] (reset! olds %&) news)]
    (apply (lift proc) cells))) 

(defn cell-map [f c]
  (let [cseq     ((lift seq) c)
        safe-nth #(try (nth %1 %2) (catch js/Error _))]
    (map #((lift (comp f safe-nth)) cseq %) (range 0 (count @cseq)))))
