;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin
  (:require
   [alandipert.desiderata :as d]
   [tailrecursion.priority-map :refer [priority-map]]))

;; specials ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn if* 
  ([pred consequent] (if pred consequent))
  ([pred consequent alternative] (if pred consequent alternative)))

(def do*          (fn [& body] (last body)))
(def throw*       #(if (string? %) (js/Error. %) %))

(defn new*
  ([class] (new class))
  ([class a] (new class a))
  ([class a b] (new class a b))
  ([class a b c] (new class a b c))
  ([class a b c d] (new class a b c d))
  ([class a b c d e] (new class a b c d e))
  ([class a b c d e f] (new class a b c d e f))
  ([class a b c d e f g] (new class a b c d e f g))
  ([class a b c d e f g h] (new class a b c d e f g h))
  ([class a b c d e f g h i] (new class a b c d e f g h i))
  ([class a b c d e f g h i & more]
    (throw (js/Error. ("new w/more than 10 args not supported in formula")))))

;; javelin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare Cell cell? input)

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

(defn destroy! [this & {:keys [sinks?]}]
  (if (or sinks? (not (seq (.-sinks this))))
    (let [srcs (.-sources this)]
      (set! (.-sources this) [])
      (doseq [src (sort-by #(.-rank %) > (filter cell? srcs))]
        (set! (.-sinks src) (disj (.-sinks src) this))
        (when-not (or (.-live src) (seq (.-sinks src))) (destroy! src))))
    (throw (js/Error. "can't destroy cell that's referenced by other cells"))))

(defn set-formula! [this & [f sources]]
  (destroy! this :sinks? true)
  (set! (.-sources this) (if f (conj (vec sources) f) (vec sources)))
  (doseq [source (filter cell? (.-sources this))]
    (set! (.-sinks source) (conj (.-sinks source) this))
    (if (> (.-rank source) (.-rank this))
      (doseq [dep (d/bf-seq identity #(.-sinks %) source)]
        (set! (.-rank dep) (next-rank)))))
  (let [compute   #(apply (deref* (peek %)) (map deref* (pop %)))
        thunk     #(set! (.-state this) (compute (.-sources this)))
        err-mesg  "formula cell can't be updated via swap! or reset!"
        watch-err (fn [_ _ _ _] (throw (js/Error. err-mesg)))
        watch-ok  (fn [_ cell _ _] (propagate! cell))]
    (-add-watch this ::propagate (if f watch-err watch-ok))
    (set! (.-thunk this) (if f thunk #(deref this)))
    (doto this propagate!)))

(deftype Cell [meta state rank prev sources sinks thunk watches live]
  cljs.core/IPrintWithWriter
  (-pr-writer [this writer opts]
    (write-all writer "#<Cell: " (pr-str state) ">"))

  cljs.core/IMeta
  (-meta [this] meta)

  cljs.core/IDeref
  (-deref [this] (.-state this))

  cljs.core/IWatchable ;; internal use only
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches] (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.-watches this) (assoc watches key f)))
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key))))

(defn cell?  [c] (= (type c) Cell))
(defn input* [x] (if (cell? x) x (input x)))
(defn input  [x] (set-formula! (Cell. {} x (next-rank) x [] #{} nil {} false)))
(defn lift   [f] (fn [& sources] (set-formula! (input ::none) f sources)))
