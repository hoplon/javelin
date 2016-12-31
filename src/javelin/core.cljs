;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.core
  (:require-macros [javelin.core])
  (:require [goog.array :as garray]
            [goog.object :as gobj]))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare Cell cell? cell input? lens? cmp-rank)

(def ^:private ^:dynamic *tx* nil)
(def ^:private last-rank (atom 0))

(defn- propagate* [pri-map]
  (when-let [next (.shift pri-map)]
    (let [old (.-prev next)
          new (if-let [f (.-thunk next)] (f) (.-state next))]
      (when (not= new old)
        (set! (.-prev next) new)
        (-notify-watches next old new)
        (let [sinks (.-sinks next)]
          (dotimes [i (alength sinks)]
            (garray/binaryInsert pri-map (aget sinks i) cmp-rank))))
      (recur pri-map))))

(defn deref*
  "If x is a Cell dereferences x and returns the value, otherwise returns x."
  [x]
  (if (cell? x) @x x))

(defn- next-rank  [ ]   (swap! last-rank inc))
(defn- cmp-rank   [a b] (let [a (.-rank a) b (.-rank b)]
                          (if (= a b) 0 (- a b))))
(defn- add-sync!  [c]   (garray/binaryInsert *tx* c cmp-rank))
(defn- safe-nth   [c i] (try (nth c i) (catch js/Error _)))
(defn- propagate! [c]   (if *tx* (doto c add-sync!) (doto c (-> array propagate*))))

;; javelin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn destroy-cell!
  "Unlinks this Cell from the cell graph and resets all internal state. Watches
  are preserved when keep-watches? is true, otherwise they are all removed."
  ([this]
   (destroy-cell! this nil))
  ([this keep-watches?]
   (let [srcs (.-sources this)]
     (set! (.-sources this) (array))
     (set! (.-update this) nil)
     (set! (.-thunk this) nil)
     (when-not keep-watches?
       (set! (.-watches this) {})
       (set! (.-numwatches this) 0))
     (dotimes [i (alength srcs)]
       (when-let [c (cell? (aget srcs i))]
         (garray/removeIf (.-sinks c) #(= % this)))))))

(defn- set-formula!* [this f sources updatefn]
  (when f
    (set! (.-constant this) true)
    (set! (.-sources this) (doto sources (.push f)))
    (dotimes [i (alength (.-sources this))]
      (let [source (aget (.-sources this) i)]
        (when (cell? source)
          (when (and (.-constant this) (not (.-constant source)))
            (set! (.-constant this) false))
          (.push (.-sinks source) this)
          (if (> (.-rank source) (.-rank this))
            (loop [q (array source)]
              (when-let [dep (.shift q)]
                (set! (.-rank dep) (next-rank))
                (recur (.concat q (.-sinks dep)))))))))
    (set! (.-thunk this) #(let [argv (.slice (.-sources this))
                                f    (deref* (.pop argv))]
                            (dotimes [i (alength argv)]
                              (aset argv i (deref* (aget argv i))))
                            (set! (.-state this) (.apply f nil argv))))
    (set! (.-update this) updatefn))
  (propagate! this))

(defn set-formula!
  "Given a Cell and optional formula function f and the cells f depends on,
  sources, updates the formula for this cell in place. If f and/or sources
  is not spcified they are set to nil."
  ([this]
   (destroy-cell! this true)
   (set-formula!* this nil nil nil))
  ([this f]
   (destroy-cell! this true)
   (set-formula!* this f (array) nil))
  ([this f sources]
   (destroy-cell! this true)
   (set-formula!* this f (into-array sources) nil))
  ([this f sources updatefn]
   (destroy-cell! this true)
   (set-formula!* this f (into-array sources) updatefn)))

(deftype Cell [meta state rank prev sources sinks thunk watches update constant numwatches]
  cljs.core/IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#object [javelin.core.Cell " (pr-str state) "]"))

  cljs.core/IWithMeta
  (-with-meta [this meta]
    (Cell. meta state rank prev sources sinks thunk watches update constant numwatches))

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
  (-notify-watches [this o n]
    (when (< 0 (.-numwatches this))
      (doseq [[key f] watches] (f key this o n))))
  (-add-watch [this k f]
    (when-not (contains? (.-watches this) k)
      (set! (.-numwatches this) (inc (.-numwatches this))))
    (set! (.-watches this) (assoc watches k f)))
  (-remove-watch [this k]
    (when (contains? (.-watches this) k)
      (set! (.-numwatches this) (dec (.-numwatches this)))
      (set! (.-watches this) (dissoc watches k)))))

(defn cell?
  "Returns c if c is a Cell, nil otherwise."
  [c]
  (when (= (type c) Cell) c))

(defn formula?
  [c]
  "Returns c if c is a formula cell, nil otherwise."
  (when (and (cell? c) (.-thunk c)) c))

(defn lens?
  [c]
  "Returns c if c is a lens, nil otherwise."
  (when (and (cell? c) (.-update c)) c))

(defn input?
  [c]
  "Returns c if c is an input cell, nil otherwise."
  (when (and (cell? c) (not (formula? c))) c))

(defn constant?
  [c]
  "Returns c if c is a constant formula cell, nil otherwise."
  (when (and (cell? c) (.-constant c)) c))

(defn set-cell!
  "Converts c to an input cell in place, setting its contents to x. It's okay
  if c was already an input cell. Changes will be propagated to any cells that
  depend on c."
  [c x]
  (set! (.-state c) x) (set-formula! c))

(defn formula
  "Returns a function that returns a formula cell with f as its formula, and
  if updatefn is provided the returned cell is a lens.

  See also: the javelin.core/cell= macro.

      (def x (cell 100))
      (def y (cell 200))

      (def z1 (cell= (+ x y)))
      (def z2 ((formula +) x y))

  The formula cells z1 and z2 are equivalent."
  ([f]
   (formula f nil))
  ([f updatefn]
   #(set-formula!* (cell ::none) f (.. js/Array -prototype -slice (call (js-arguments))) updatefn)))

(defn lens
  "Returns a new lens whose value is the same as c's with update function f.
  This is equivalent to ((formula identity f) c)."
  [c f]
  ((formula identity f) c))

(defn cell
  "Returns a new input cell containing value x. The :meta option can be used
  to create the cell with the given metadata map."
  ([x] (Cell. nil x (next-rank) x (array) (array) nil {} nil false 0))
  ([x & {:keys [meta]}] (Cell. meta x (next-rank) x (array) (array) nil {} nil false 0)))

(def ^:deprecated lift
  "This function is deprecated, please use #'javelin.core/formula instead."
  formula)

;; javelin util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dosync*
  "Calls the thunk with no arguments within a transaction. Propagation of
  updates to formula cells is deferred until the transaction is complete.
  Input cells *will* update during the transaction. Transactions may be
  nested.

  See also: the javelin.core/dosync macro."
  [thunk]
  (if *tx*
    (thunk)
    (binding [*tx* (array)]
      (thunk)
      (let [tx *tx*]
        (binding [*tx* nil]
          (propagate* tx))))))

(defn alts!
  "Given a number of cells, returns a formula cell whose value is a seq of
  values from cells that changed in the last update. Note that multiple cells
  may update atomically, which is why the formula's value is a seq.

  Consider:

      (def a (cell {:x 1 :y 2}))
      (def x (cell= (:x a)))
      (def y (cell= (:y a)))
      (def z (alts! x y))

  then,

      (deref z) ;=> (1 2)

      (swap! a assoc :x 42)
      (deref z) ;=> (42)

      (reset! a {:x 10 :y 20})
      (deref z) ;=> (10 20)
  "
  [& cells]
  (let [olds    (atom (repeat (count cells) ::none))
        tag-neq #(vector (not= %1 %2) %2)
        diff    #(->> %2 (map tag-neq %1) (filter first) (map second) distinct)
        proc    #(let [news (diff (deref olds) %&)] (reset! olds %&) news)]
    (apply (formula proc) cells)))

(defn cell-map
  "Given a function f and a cell c that contains a seqable collection of items,
  returns a seq of formula cells such that the ith formula cell is equivalent
  to (cell= (f (nth c i)))."
  [f c]
  (let [cseq ((formula seq) c)]
    (map #((formula (comp f safe-nth)) cseq %) (range 0 (count @cseq)))))

(defn cell-doseq*
  "Given a function f and a cell c that contains a seqable collection of items,
  calls f for side effects once for each item in c, passing one argument: a
  formula cell equivalent to (cell= (nth c i)) for the ith item in c. Whenever
  c grows beyond its previous maximum size f is called as above for each item
  beyond the maximum size. Nothing happens when c shrinks.

  See also: the javelin.core/cell-doseq macro.

  Consider:

      (def things (cell [:a :b :c]))
      (cell-doseq*
        things
        (fn doit [x]
          (prn :creating @x)
          (add-watch x nil #(prn :updating %3 %4))))

      ;; the following is printed:

      :creating :a
      :creating :b
      :creating :c

  Shrink things by removing the last item:

      (swap! things pop)

      ;; the following is printed (because the 3rd item in things is now nil,
      ;; since things only contains 2 items) -- note that the doit function is
      ;; not called (or we would see a :creating message):

      :updating :c nil

  Grow things such that it is one item larger than it ever was:

      (swap! things into [:u :v])

      ;; the following is printed (because things now has 4 items, so the 3rd
      ;; item is now :u and the max size increases by one with the new item :v):

      :updating nil :u
      :creating :v

  A weird imagination is most useful to gain full advantage of all the features."
  [c f]
  (let [pool-size (atom 0)]
    (-> c ((formula (fn [items]
                      (let [cnt (count items)]
                        (when (< @pool-size cnt)
                          (dotimes [i (- cnt @pool-size)]
                            (f ((formula safe-nth) c (+ i @pool-size))))
                          (reset! pool-size cnt)))))))))
