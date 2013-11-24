(ns tailrecursion.javelin-clj
  (:refer-clojure :exclude [accessor])
  (:require
    [riddley.compiler           :refer [locals]]
    [riddley.walk               :refer [walk-exprs]]
    [clojure.data.priority-map  :refer [priority-map]]))

(defn bf-seq [branch? children root]
  (let [walk (fn walk [queue]
               (when-let [node (peek queue)]
                 (lazy-seq
                  (cons node (walk (into (pop queue)
                                         (when (branch? node)
                                           (children node))))))))]
    (walk (conj clojure.lang.PersistentQueue/EMPTY root))))

(defn make-cell [meta state rank prev sources sinks thunk]
  (->>
    {::rank     rank
     ::prev     prev
     ::sources  sources
     ::sinks    sinks
     ::thunk    thunk}
    (merge meta)
    (atom state :meta)))

(defn accessor [key]
  (fn
    ([this] (get (meta this) key))
    ([this val] (alter-meta! this assoc key val))))

(def cell?    #(contains? (meta %) ::rank))
(def rank     (accessor ::rank))
(def prev     (accessor ::prev))
(def sources  (accessor ::sources))
(def sinks    (accessor ::sinks))
(def thunk    (accessor ::thunk))

(def  last-rank     (atom 0))
(defn next-rank [ ] (swap! last-rank inc))
(defn deref*    [x] (if (cell? x) @x x))

(defn propagate! [cell]
  (loop [queue (priority-map cell (rank cell))]
    (when (seq queue)
      (let [next      (key (peek queue))
            value     ((thunk next))
            continue? (not= value (prev next))
            reducer   #(assoc %1 %2 (rank %2))
            siblings  (pop queue)
            children  (sinks next)]
        (if continue? (prev next value))
        (recur (if continue? (reduce reducer siblings children) siblings))))))

(defn destroy-cell! [this]
  (let [srcs (sources this)]
    (sources this [])
    (doseq [src (filter cell? srcs)]
      (sinks src (disj (sinks src) this)))))

(defn set-formula! [this & [f srcs]]
  (destroy-cell! this)
  (sources this (if f (conj (vec srcs) f) (vec srcs)))
  (doseq [source (filter cell? (sources this))]
    (sinks source (conj (sinks source) this))
    (if (> (rank source) (rank this))
      (doseq [dep (bf-seq identity #(sinks %) source)]
        (rank dep (next-rank)))))
  (let [compute #(apply (deref* (peek %)) (map deref* (pop %)))
        thunk*  #(reset! this (compute (sources this)))]
    (if f
      (remove-watch this ::cell)
      (add-watch this ::cell (fn [_ c _ _] (propagate! c))))
    (thunk this (if f thunk* #(deref this)))
    (doto this propagate!)))

(defn cell      [x]   (set-formula! (make-cell {} x (next-rank) x [] #{} nil)))
(defn lift      [f]   (fn [& sources] (set-formula! (cell ::none) f sources)))
(defn set-cell! [c x] (doto c (remove-watch ::cell) (reset! x) (set-formula!)))

(def specials '#{if def do loop* letfn* throw try recur new set! ns
                 deftype* defrecord* & monitor-enter monitor-exit case*})

(defmacro cell= [expr]
  (let [hoist   (atom [])
        local   #(symbol (name %))
        core?   #(= "clojure.core" (namespace %))
        skip?   #(or (contains? (locals) %) (contains? specials %) (core? %))
        walk!   #(do (if-not (skip? %) (do (swap! hoist conj %) (local %)) %))
        walked  (walk-exprs symbol? walk! expr)
        hoisted (distinct @hoist)]
    `((lift (fn ~(mapv local hoisted) ~walked)) ~@hoisted)))

(defmacro defc  [name val]  `(def ~name (cell ~val)))
(defmacro defc= [name expr] `(def ~name (cell= ~expr)))
