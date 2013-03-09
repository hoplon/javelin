;   Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin.macros
  (:require [clojure.walk    :refer [macroexpand-all postwalk-replace prewalk postwalk]]
            [cljs.analyzer   :as a]
            [clojure.java.io :as io]
            [clojure.string  :as s]
            [cljs.compiler   :refer [forms-seq]]))

(defmacro with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's a cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(create-ns 'tailrecursion.javelin.core)

(let [home      #(symbol "tailrecursion.javelin" %)
      keyw      #(keyword "tailrecursion.javelin.core" %)
      specials  #{'if 'def 'do 'loop* 'letfn* 'throw 'try* 'recur 'new 'set!
                  'ns 'deftype* 'defrecord* '&}
      to-list   #(into '() (reverse %))
      lift      (home "lift")
      input     (home "input")
      input*    (home "input*")
      deref*    (home "deref*")
      selfkey   (keyw "self")
      special?  #(if (contains? specials %) (home (str % "*")))
      unquote?  #(and (seq? %) (= 'clojure.core/unquote (first %)))
      quoted?   #(and (seq? %) (= 'quote (first %)))
      input?    #(and (seq? %) (= input (first %)))
      func?     #(and (seq? %) (= 'fn* (first %)))
      let*?     #(and (seq? %) (= 'let* (first %)))
      js*?      #(and (seq? %) (= 'js* (first %)))
      dot?      #(and (seq? %) (= '. (first %)))
      lifted-f? #(and (seq? %)
                      (= lift (first %))
                      (= 'fn* (first (second %)))
                      (= 2 (count %)))
      listy?    #(or (list? %)
                     (= clojure.lang.LazySeq (type %))
                     (= clojure.lang.Cons (type %)))]

  (declare do-lift)

  (defn do-map
    [form]
    (do-lift (to-list `(hash-map ~@(mapcat identity form)))))

  (defn do-vector
    [form]
    (do-lift (to-list `(vector ~@form))))

  (defn do-set
    [form]
    (do-lift (to-list `(set ~@form))))

  (defn do-let*
    [[_ bindings & body]] 
    `(let* ~(mapv #(%1 %2) (cycle [identity do-lift]) bindings)
       ~@(map do-lift body)))

  (defn do-self
    [form]
    `(~input ~{selfkey `(~deref* ~(do-lift (second form)))}))

  (defn do-js*
    [[_ tmpl & args]]
    (let [bindings (mapv (fn [_] (gensym)) args)]
      (do-lift `((fn* ~bindings (~'js* ~tmpl ~@bindings)) ~@args))))

  (defn do-dot
    [[_ obj meth & args]]
    (let [bindings (map (fn [_] (gensym)) args)]
      (do-lift `((fn* [obj# ~@bindings] (~'. obj# ~meth ~@bindings)) ~obj ~@args))))

  (defn do-lift
    [form]
    (cond
      (map? form)         (do-map form)
      (vector? form)      (do-vector form)
      (set? form)         (do-set form)
      (not (listy? form)) form
      (unquote? form)     (do-self form)
      (quoted? form)      (second form)
      (func? form)        (list input form)
      (let*? form)        (do-let* form)
      (js*? form)         (do-js* form)
      (dot? form)         (do-dot form)
      :else               (let [[op & args] form]
                            (if (= op 'apply)
                              `(apply (~lift ~(do-lift (first args))) ~@(map do-lift (rest args)))
                              `((~lift ~(do-lift (or (special? op) op))) ~@(map do-lift args))))))

  (defn macroexpand*
    [env form]
    (let [ex (a/macroexpand-1 env form)]
      (if (identical? ex form)
        form
        (macroexpand* env ex))))

  (defn macroexpand-all*
    [env form]
    (prewalk (fn [x] (if (seq? x) (macroexpand* env x) x)) form))

  (defmacro mx
    [form]
    (pr-str (macroexpand-all* &env form)))

  (defmacro cfn
    [bindings & body]
    `(fn [& args#]
       ((fn ~bindings ~@body) (map input* args#))))

  (defmacro defcfn
    [name bindings & body]
    `(def ~name (cfn ~bindings ~@body)))

  (defmacro cell
    [form]
    (let [form    (macroexpand-all* &env form)
          lifted  (do-lift form)
          q?      (or (quoted? form) (not (listy? lifted)))
          expr    (if q? (list input lifted) lifted)]
      expr)))




;; mirroring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nsym->path
  [sym]
  (-> (str sym)
      (s/replace "." java.io.File/separator)
      (s/replace "-" "_")
      (str ".cljs")))

(defn all-list-forms
  [forms-seq]
  (let [x (atom [])]
    (postwalk #(do (if (list? %) (swap! x conj %)) %) forms-seq)
    (seq @x)))

(defn defs-in
  [def-or-defn sym]
  (let [ns-file (io/resource (nsym->path sym))]
    (->> (all-list-forms (forms-seq ns-file))
         (filter (comp (partial = def-or-defn) first))
         (mapv second))))

(defn mirrored-defs
  [ns-sym]
  (let [remote-defs (defs-in 'def ns-sym)]
    (map (fn [r] `(def ~r ~(symbol (str ns-sym) (str r)))) remote-defs)))

(defn mirrored-defns
  [ns-sym]
  (let [remote-defns (defs-in 'defn ns-sym)]
    (map (fn [r] `(defn ~r [& args#]
                    (apply ~(symbol (str ns-sym) (str r)) args#)))
         remote-defns)))

(defmacro mirror
  "Mirrors all public defs and defns in the remote namespace
  represented by ns-sym in whatever the current namespace is.  The
  remote namespace must have been required in order for advanced
  compilation to work."
  [ns-sym]
  `(do ~@(mirrored-defs  ns-sym)
       ~@(mirrored-defns ns-sym)))

;; test related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro are=
  ([a b]
     `(assert (= ~a ~b)))
  ([a b c & more]
     `(do (are= ~a ~b) (are= ~c ~@more))))

(comment

  (URI. foo)
  (def trans (partial apply mapv vector))

  (def mxpp #(do (clojure.pprint/pprint %) (println ";; macroexpands to...")
               (clojure.pprint/pprint (clojure.walk/macroexpand-all %))))

  (let [m (apply sorted-map (mapcat identity (vec {3 :c 1 :a 2 :b})))]
    (interleave (keys m) (vals m))
    )
  (vec #{1 2 3})
  (list* :a [:b :c])
  (clojure.pprint/pprint (into #{} (keys clojure.lang.Compiler/specials)))
  (macroexpand-all '(foo ~(b a) a))
  (mxpp '(cell (case foo "hello" bar "world" other :not-found)))
  (mxpp '(cell (condp = foo "hello" bar "world" other :not-found)))
  (mxpp '(cell (if (= a 0) (cell a) (cell b))))
  (mxpp '(cell 0))
  (mxpp '(cell '(+ a 5)))
  (mxpp '(cell ''a))
  (mxpp '(cell {foo [a b 7]}))
  (mxpp '(cell #{foo [a b 7]}))
  (mxpp '(cell (+ a ~0)))
  (mxpp '(cell (+ a ~(+ b c))))
  (mxpp '(cell #(+ %1 %2)))
  (mxpp '(cell (#(+ %1 %2) a b)))
  (mxpp '(cell (if-not (pred? a) b c)))

  )
