(ns javelin.cell
  (:require
    [clojure.set :refer [difference intersection]]
    [javelin.kahnsort :refer [topo-sort]]
    [riddley.compiler :refer [locals]]
    [riddley.walk :refer [walk-exprs special-form?]])
  (:import
    [javelin Cell]
    [java.lang.ref WeakReference]))

(defn cell?
  [x]
  (when (instance? Cell x) x))

(defn formula?
  [x]
  (when (and (cell? x) (seq @(.-cellSources x))) x))

(defn input?
  [x]
  (when (and (cell? x) (not (formula? x))) x))

(defn lens?
  [x]
  (when (and (formula? x) @(.-cellSetter x)) x))

(defn cell
  [x & {:keys [meta]}]
  (Cell/input x meta))

(defn formula
  ([thunk sources] (formula thunk sources nil))
  ([thunk sources setter] (Cell/formula thunk (vec sources) setter)))

(defn lens
  [c setter]
  (formula identity [c] setter))

(defn- analyze
  [expr env]
  (let [bindings (atom {})
        core-re  #"^(clojure|java)\."
        re?      #(boolean (re-find %2 %1))
        isop     #(and (seq? %2) (= %1 (first %2)))
        lookup   #(and (symbol? %) (ns-resolve *ns* %))
        hoist!   #(do (swap! bindings assoc %1 %2) %1)
        dot?     (partial isop '.)
        pass1?   (partial isop 'clojure.core/unquote)
        pass2?   (partial isop 'clojure.core/unquote-splicing)
        class?   #(some-> % lookup type (= java.lang.Class))
        core?    #(some-> % lookup meta :ns ns-name str (re? core-re))
        local?   #(and (contains? (locals) %) (not (contains? env %)))
        walk?    (every-pred
                   (some-fn symbol? dot? pass1? pass2?)
                   (complement (some-fn core? class? local? special-form?)))]
    (letfn [(dot! [[sym obj meth & more]]
              (let [obj    (walk obj)
                    seqm?  (seq? meth)
                    more   (mapv walk (if seqm? (rest meth) more))
                    meth   (if seqm? (first meth) meth)
                    args   (list* meth more)]
                `(~sym ~obj ~@(if seqm? [args] args))))
            (walk! [x]
              (cond (dot?   x) (dot! x)
                    (pass1? x) (hoist! (gensym) (second x))
                    (pass2? x) (hoist! (gensym) `(deref ~(second x)))
                    :otherwise (hoist! (symbol (name x)) x)))
            (walk [x] (walk-exprs walk? walk! x))]
      [(walk expr) @bindings])))

(defmacro cell=
  ([expr]
   `(cell= ~expr nil))
  ([expr setter]
   (let [[expr binds] (analyze expr &env)]
     `(formula (fn [~@(keys binds)] ~expr) [~@(vals binds)] ~setter))))

(defmacro defc  [name val]  `(def ~name (cell ~val)))
(defmacro defc= [name expr & [setter]] `(def ~name (cell= ~expr ~setter)))

(defn- add-sink
  [c sinks]
  (let [sinks (conj (keep (memfn get) sinks) c)
        srcs  #(filter cell? (ensure (.-cellSources %)))]
    (->> (map #(vector % (set (srcs %))) sinks)
         (into {})
         (topo-sort)
         (reverse)
         (filter (set sinks))
         (mapv #(WeakReference. %)))))

(defn- remove-sink
  [c sinks]
  (vec (remove (comp #{c} (memfn get)) sinks)))

(defn cell!
  [c x]
  (dosync
    (doseq [s (ensure (.-cellSources c)) :when (cell? s)]
      (alter (.-cellSinks s) (partial remove-sink c)))
    (ref-set (.-cellSources c) [])
    (ref-set c x)))

(defn formula!
  ([c f sources]
   (formula! c f sources nil))
  ([c f sources setter]
   (dosync
     (let [src  (conj sources f)
           src' (ensure (.-cellSources c))]
       (ref-set (.-cellSources c) src)
       (ref-set (.-cellSetter c) setter)
       (doseq [s src :when (cell? s)]
         (alter (.-cellSinks s) (partial add-sink c)))
       (doseq [s (difference (set src') (set src)) :when (cell? s)]
         (alter (.-cellSinks s) (partial remove-sink c)))
       (doto c (-> (.cellUpdate) (when (.cellPropagate c))))))))

(defmacro cell!=
  ([c expr]
   `(cell!= ~c ~expr nil))
  ([c expr setter]
   (let [[expr binds] (analyze expr &env)]
     `(formula! ~c (fn [~@(keys binds)] ~expr) [~@(vals binds)] ~setter))))

(defmacro ^:private defalias
  [old-name new-var]
  `(let [v# ~new-var
         m# (-> v# meta (select-keys [:arglists :doc :macro]))]
     (intern *ns* (with-meta '~old-name m#) v#)))

(defalias formula-of
  (defmacro fn=
    ([[& refs] expr]
     `(fn= [~@refs] ~expr nil))
    ([[& refs] expr setter]
     `(formula (fn [~@refs] ~expr) [~@refs] ~setter))))

(defalias formula-of!
  (defmacro fn!=
    ([c [& refs] expr]
     `(fn!= ~c [~@refs] ~expr nil))
    ([c [& refs] expr setter]
     `(formula! ~c (fn [~@refs] ~expr) [~@refs] ~setter))))

(defalias formulet
  (defmacro let=
    ([[& bindings] expr]
     `(let= [~@bindings] ~expr nil))
    ([[& bindings] expr setter]
     (let [[syms vals] (apply map list (partition 2 bindings))]
       `(formula (fn [~@syms] ~expr) [~@vals] ~setter)))))

(defalias formulet!
  (defmacro let!=
    ([c [& bindings] expr]
     `(let!= ~c [~@bindings] expr nil))
    ([c [& bindings] expr setter]
     (let [[syms vals] (apply map list (partition 2 bindings))]
       `(formula! ~c (fn [~@syms] ~expr) [~@vals] ~setter)))))

(defn- extract-syms
  "Extract symbols that will be bound by bindings, including autogenerated
  symbols produced for destructuring."
  [bindings]
  (map first (partition 2 (destructure bindings))))

(defn- extract-syms-without-autogen
  "Extract only the symbols that the user is binding from bindings, omitting
  any intermediate autogenerated bindings used for destructuring. A trick is
  used here taking advantage of the fact that gensym names are produced as a
  side effect -- successive calls to extract-syms are not redundant."
  [bindings]
  (let [syms1 (set (extract-syms bindings))
        syms2 (set (extract-syms bindings))]
    (seq (intersection syms1 syms2))))

(defn- bind-syms
  "Given a binding form, returns a seq of the symbols that will be bound.

  (bind-syms '[{:keys [foo some.ns/bar] :as baz} baf & quux])
  ;=> (foo bar baz baf quux)"
  [form]
  (extract-syms-without-autogen [form nil]))

(defn- safe-nth
  [& args]
  (try (apply nth args) (catch Throwable t)))

(defn cell-map
  "Given a function f and a cell c that contains a seqable collection of items,
  returns a seq of formula cells such that the ith formula cell is equivalent
  to (cell= (f (nth c i)))."
  [f c]
  (let [cseq (formula seq [c])]
    (map #(formula (comp f safe-nth) [cseq %]) (range 0 (count @cseq)))))

(defmacro cell-let-1
  [[bindings c] & body]
  (let [syms  (bind-syms bindings)
        dcell `(formula (fn [~bindings] [~@syms]) [~c])]
    `(let [[~@syms] (cell-map identity ~dcell)] ~@body)))

(defmacro cell-let
  [[bindings c & more] & body]
  (if-not (seq more)
    `(cell-let-1 [~bindings ~c] ~@body)
    `(cell-let-1 [~bindings ~c] (cell-let ~(vec more) ~@body))))

(defn cell-doseq*
  "Given a function f and a cell c that contains a seqable collection of items,
  calls f for side effects once for each item in c, passing one argument: a
  formula cell equivalent to (cell= (nth c i)) for the ith item in c. Whenever
  c grows beyond its previous maximum size f is called as above for each item
  beyond the maximum size. Nothing happens when c shrinks. A weird imagination
  is most useful to gain full advantage of all the features."
  [c f]
  (let [pool-size (atom 0)]
    (->> [c] (formula (fn [items]
                        (let [cnt (count items)]
                          (when (< @pool-size cnt)
                            (dotimes [i (- cnt @pool-size)]
                              (f (formula safe-nth [c (+ i @pool-size)])))
                            (reset! pool-size cnt))))))))

(defmacro cell-doseq
  "Takes a vector of binding-form/collection-cell pairs and one or more body
  expressions, similar to clojure.core/doseq. Iterating over the collection
  cells produces a sequence of items that may grow, shrink, or update over
  time. Whenever this sequence grows the body expressions are evaluated (for
  side effects) exactly once for each new location in the sequence. Bindings
  are bound to cells that refer to the item at that location. A weird imagin-
  ation is most useful to gain full advantage of all the features."
  [bindings & body]
  `(cell-doseq*
     (formula seq [~(second bindings)])
     (fn [item#] (cell-let [~(first bindings) item#] ~@body))))
