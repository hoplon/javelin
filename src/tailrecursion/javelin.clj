;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin
  (:require [clojure.walk    :refer [prewalk]]
            [clojure.pprint  :as p]
            [cljs.analyzer   :as a]
            [clojure.java.io :as io]
            [clojure.string  :as s]))

(declare walk)

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's a cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(defn make-sane [form]
  (binding [*print-meta* true]
    (read-string (pr-str form))))

(defn macroexpand* [env form]
  (if (seq? form)
    (let [ex (a/macroexpand-1 env form)]
      (if (identical? ex form)
        form
        (macroexpand* env ex)))
    form))

(defn macroexpand-all* [env form]
  (make-sane (prewalk (partial macroexpand* env) form)))

(defmacro macroexpand-all [form]
  (macroexpand-all* &env form))

(defmacro mx [form]
  `(println
     ~(with-out-str
        (p/write (macroexpand-all* &env form) :dispatch p/code-dispatch))))

(defmacro mx2 [form]
  `(println
     ~(with-out-str
        (p/write (macroexpand-all* &env form)))))

;; javelin cells ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *env*    nil)
(def ^:dynamic *hoist*  nil)
(def ^:dynamic *pass*   nil)

(let [to-list   #(into '() (reverse %))
      cell'     (symbol "tailrecursion.javelin" "cell")
      lift'     (symbol "tailrecursion.javelin" "lift")
      set-frm'  (symbol "tailrecursion.javelin" "set-formula!")
      special   '#{if def fn* do let* loop* letfn* throw try
                   recur new set! ns deftype* defrecord* . js* & quote}
      listy?    #(or (list? %)
                     (= clojure.lang.LazySeq (type %))
                     (= clojure.lang.Cons (type %)))
      special?  #(contains? special %)
      unsupp?*  #(contains? '#{def ns deftype* defrecord*} %)
      core?     #(contains? #{"clojure.core" "cljs.core"} (namespace %))
      empty?*   #(= 0 (count %))
      dot?      #(= '. (first %))
      binding1? #(contains? '#{let* loop*} (first %))
      binding2? #(= 'letfn* (first %))
      binding3? #(= 'fn* (first %))
      quoted?   #(= 'quote (first %))
      unwrap1?  #(= 'clojure.core/unquote (first %))
      unwrap2?  #(= 'clojure.core/unquote-splicing (first %))
      err1      #(format "formula expansion contains unsupported %s form" %)]

  (defn unsupp? [x local]
    (let [op (first x)]
      (and (not (*env* op)) (not (local op)) (unsupp?* op))))

  (defn walk-sym [x local]
    (if-not (and (not (local x)) (not (core? x)) (or (*env* x) (not (special? x))))
      x
      (let [h (@*hoist* x)]
        (when-not h (swap! *hoist* conj (with-meta x {::h (gensym)})))
        (::h (meta (@*hoist* x))))))

  (defn walk-map [x local]
    (into (empty x) (map #(mapv (fn [x] (walk x local)) %) x)))

  (defn walk-seq [x local]
    (into (empty x) (map #(walk % local) x)))

  (defn walk-bind1 [[sym bindings & body] local]
    (let [local     (atom local)
          bind1     (fn [[k v]]
                      (with-let [x [k (walk v @local)]]
                        (swap! local conj k)))
          bindings  (mapcat bind1 (partition 2 bindings))]
      (to-list `(~sym [~@bindings] ~@(map #(walk % @local) body)))))

  (defn walk-bind2 [[sym bindings & body] local]
    (let [local     (reduce conj local (map first (partition 2 bindings)))
          bindings  (map #(%1 %2) (cycle [identity #(walk % local)]) bindings)]
      (to-list `(~sym [~@bindings] ~@(map #(walk % local) body)))))

  (defn walk-bind3 [[sym & arities] local]
    (let [fname   (when (symbol? (first arities)) [(first arities)])
          arities (if fname (rest arities) arities)
          arities (if (vector? (first arities)) [arities] arities)
          local   (if fname (conj local (first fname)) local)]
      (let [mkarity (fn [[bindings & body]]
                      (let [local (into local (remove #(= '& %) bindings))]
                        (to-list `([~@bindings] ~@(map #(walk % local) body)))))
            arities (map mkarity arities)]
        (to-list `(~sym ~@fname ~@arities)))))

  (defn walk-passthru [x local]
    (with-let [s (gensym)] (swap! *pass* assoc s x)))

  (defn walk-dot [[sym obj meth & more] local]
    (let [obj       (walk obj local)
          more      (map #(walk % local) more)
          walk-meth (fn [m] (list (first m) (map #(walk % local) (rest m))))]
      (to-list `(~sym ~obj ~@(if-not (listy? meth) `[~meth ~@more] [(walk-meth meth)])))))

  (defn walk-list [x local]
    (let [unsupp? #(unsupp? % local)]
      (cond (empty?*   x) x
            (dot?      x) (walk-dot x local)
            (binding1? x) (walk-bind1 x local)
            (binding2? x) (walk-bind2 x local)
            (binding3? x) (walk-bind3 x local)
            (quoted?   x) (walk-passthru x local)
            (unwrap1?  x) (walk-passthru (second x) local) 
            (unwrap2?  x) (walk-passthru (list 'deref (second x)) local) 
            (unsupp?   x) (throw (Exception. (err1 (first x))))
            :else         (to-list (map #(walk % local) x)))))

  (defn walk [x local]
    (cond (symbol? x) (walk-sym x local)
          (map?    x) (walk-map x local)
          (set?    x) (walk-seq x local)
          (vector? x) (walk-seq x local)
          (listy?  x) (walk-list x local)
          :else       x))

  (defn hoist [x env]
    (binding [*env* env, *hoist* (atom #{}), *pass* (atom {})]
      (let [body          (walk (macroexpand-all* env x) #{})
            [params args] (if (empty? @*pass*) [[] []] (apply map vector @*pass*))
            params        (into params (map #(::h (meta %)) @*hoist*))
            args          (into args @*hoist*)]
        [(list 'fn params body) args])))

  (defn cell* [x env]
    (let [[f args] (hoist x env)]
      (to-list `((~lift' ~f) ~@args))))

  (defn set-cell* [c x env]
    (let [[f args] (hoist x env)]
      (list set-frm' c f args)))

  (defmacro cell=      [expr]         (cell* expr &env))
  (defmacro set-cell!= [c expr]       (set-cell* c expr &env))
  (defmacro defc      ([sym expr]     `(def ~sym (~cell' ~expr)))
                      ([sym doc expr] `(def ~sym ~doc (~cell' ~expr))))
  (defmacro defc=     ([sym expr]     `(def ~sym (cell= ~expr)))
                      ([sym doc expr] `(def ~sym ~doc (cell= ~expr)))))

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
