;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin.macros
  (:require [clojure.walk    :refer [prewalk]]
            [cljs.analyzer   :as a]
            [clojure.java.io :as io]
            [clojure.string  :as s]))

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's a cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(defn macroexpand*
  [env form]
  (if (seq? form)
    (let [ex (a/macroexpand-1 env form)]
      (if (identical? ex form)
        form
        (macroexpand* env ex)))
    form))

(defn macroexpand-all*
  [env form]
  (prewalk (partial macroexpand* env) form))

(defmacro macroexpand-all
  [form]
  (macroexpand-all* &env form))

;; javelin cells ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-ns 'tailrecursion.javelin)

(let [home      #(symbol "tailrecursion.javelin" %)
      keyw      #(keyword "tailrecursion.javelin" %)
      specials  #{'if 'def 'do 'loop* 'letfn* 'throw 'try* 'recur 'new 'ns 'deftype* 'defrecord* '&}
      to-list   #(into '() (reverse %))
      cell?*    (home "cell?")
      lift      (home "lift")
      input     (home "input")
      input*    (home "input*")
      deref*    (home "deref*")
      set-frm!  (home "set-formula!")
      destroy!  (home "destroy!")
      special?  #(if (contains? specials %) (home (str % "*")))
      unquote?  #(and (seq? %) (= 'clojure.core/unquote (first %)))
      unsplice? #(and (seq? %) (= 'clojure.core/unquote-splicing (first %)))
      quote?    #(and (seq? %) (= 'quote (first %)))
      input?    #(and (seq? %) (= input (first %)))
      func?     #(and (seq? %) (= 'fn* (first %)))
      let*?     #(and (seq? %) (= 'let* (first %)))
      js*?      #(and (seq? %) (= 'js* (first %)))
      dot?      #(and (seq? %) (= '. (first %)))
      set!?     #(and (seq? %) (= 'set! (first %)))
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

  (defn do-js*
    [[_ tmpl & args]]
    (let [bindings (mapv (fn [_] (gensym)) args)]
      (do-lift `((fn* ~bindings (~'js* ~tmpl ~@bindings)) ~@args))))

  (defn do-dot
    [[_ obj meth & args]]
    (let [bindings (map (fn [_] (gensym)) args)]
      (do-lift `((fn* [obj# ~@bindings] (~'. obj# ~meth ~@bindings)) ~obj ~@args))))

  (defn do-set!
    [[_ target val]]
    (do-lift `((fn* [val#] (set! ~target val#)) ~val)))

  (defn do-lift
    [form]
    (cond
      (map? form)         (do-map form)
      (vector? form)      (do-vector form)
      (set? form)         (do-set form)
      (not (listy? form)) form
      (quote? form)       form
      (unquote? form)     (second form)
      (unsplice? form)    (list deref (second form))
      (func? form)        (list input form)
      (let*? form)        (do-let* form)
      (js*? form)         (do-js* form)
      (dot? form)         (do-dot form)
      (set!? form)        (do-set! form)
      :else               (let [[op & args] form]
                            (if (= op 'apply)
                              `(apply (~lift ~(do-lift (first args))) ~@(map do-lift (rest args)))
                              `((~lift ~(do-lift (or (special? op) op))) ~@(map do-lift args))))))

  (defn mark-live
    [form]
    `(let [c# ~form] (set! (.-live c#) true) c#))

  (defmacro cell?
    [c]
    `(~cell?* ~c))

  (defmacro set-cell!
    [c form]
    `(do (set! (.-state ~c) ~form) (~set-frm! ~c)))

  (defmacro set-cell!=
    [c form]
    `(~set-frm! ~c identity [~(do-lift (macroexpand-all* &env form))]))

  (defmacro destroy-cell!
    [c]
    `(~destroy! ~c))

  (defmacro cell
    "Create an input cell using form for initial value."
    [form]
    (mark-live `(~input ~form)))

  (defmacro cell=
    "Create formula cell using form as the formula expression."
    [form]
    (mark-live (do-lift (macroexpand-all* &env form)))))

;; mirroring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nsym->path
  [sym]
  (-> (str sym)
      (s/replace "." java.io.File/separator)
      (s/replace "-" "_")
      (str ".cljs")))

(defn all-list-forms
  [forms]
  (filter list? (tree-seq coll? seq forms)))

(defn resource*
  [path]
  (or (io/resource path) (io/file "src/cljs" path)))

(defn read-file
  [f]
  (with-open [in (java.io.PushbackReader. (io/reader f))]
    (->> (repeatedly #(read in false ::eof))
         (take-while (partial not= ::eof))
         doall)))

(defn ops-in
  [op-sym sym]
  (let [ns-file (resource* (nsym->path sym))]
    (->>
     (read-file ns-file)
     list*
     (tree-seq coll? seq)
     (filter list?)
     (filter (comp (partial = op-sym) first))
     (mapv second))))

(defn mirrored-defs
  [ns-sym]
  (let [remote-defs (ops-in 'def ns-sym)]
    (map (fn [r] `(def ~r ~(symbol (str ns-sym) (str r)))) remote-defs)))

(defn mirrored-defns
  [ns-sym]
  (let [remote-defns (ops-in 'defn ns-sym)]
    (map (fn [r] `(defn ~r [& args#]
                    (apply ~(symbol (str ns-sym) (str r)) args#)))
         remote-defns)))

(defmacro mirror
  "Mirrors all public defs and defns in the remote namespace
  represented by ns-sym in whatever the current namespace is.
  The remote namespace must have been required in order for
  advanced compilation to work."
  [ns-sym]
  `(do ~@(mirrored-defs  ns-sym)
       ~@(mirrored-defns ns-sym)))

;; test related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro are=
  ([a b]
     `(assert (= ~a ~b)))
  ([a b c & more]
     `(do (are= ~a ~b) (are= ~c ~@more))))
