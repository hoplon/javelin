;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.macros
  (:refer-clojure :exclude [dosync])
  (:require
    [clojure.walk    :as w :refer [prewalk]]
    [clojure.pprint  :as p]
    [clojure.java.io :as io]
    [clojure.string  :as s]))

(declare walk)

(def ^:dynamic *cljs* false)

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's a cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(defn bind-syms [form]
  (let [sym? #(and (symbol? %) (not= '& %))]
    (->> form (tree-seq coll? seq) (filter sym?) distinct)))

(try (require 'cljs.analyzer)
     (catch java.io.FileNotFoundException e))

(defn macroexpand* [env form]
  (if (seq? form)
    (let [ex ((resolve 'cljs.analyzer/macroexpand-1) env form)]
      (if (identical? ex form)
        form
        (macroexpand* env ex)))
    form))

(defn macroexpand-all* [env form]
  (if *cljs*
    (prewalk (partial macroexpand* env) form)
    (w/macroexpand-all form)))

(defmacro macroexpand-all [form]
  (if *cljs*
    (macroexpand-all* &env form)
    (w/macroexpand-all form)))

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

(create-ns 'js)

(def cljs-specials '#{if def fn* do let* loop* letfn* throw try recur new set!
                      ns deftype* defrecord* . js* & quote case* var})

(def clj-specials '#{& monitor-exit case* try reify* finally loop* do letfn* if
                     clojure.core/import* new deftype* let* fn* recur set! . var
                     quote catch throw monitor-enter def})

(let [to-list   #(into '() (reverse %))
      special   (if *cljs* cljs-specials clj-specials)
      special?  #(contains? special %)
      unsupp?*  #(contains?
                  (if *cljs*
                    '#{def ns deftype* defrecord*}
                    '#{def ns deftype defrecord})
                  %)
      core?     #(contains? #{"clojure.core" "cljs.core" "js"} (namespace %))
      empty?*   #(= 0 (count %))
      dot?      #(= '. (first %))
      try?      #(= 'try (first %))
      catch?    #(= 'catch (first %))
      finally?  #(= 'finally (first %))
      binding1? #(contains? '#{let* loop*} (first %))
      binding2? #(= 'letfn* (first %))
      binding3? #(= 'fn* (first %))
      catch?    #(= 'catch (first %))
      quoted?   #(= 'quote (first %))
      unwrap1?  #(= 'clojure.core/unquote (first %))
      unwrap2?  #(= 'clojure.core/unquote-splicing (first %))
      err1      #(format "formula expansion contains unsupported %s form" %)]

  (defn unsupp? [x local]
    (let [op (first x)]
      (and (not (*env* op)) (not (local op)) (unsupp?* op))))

  (defn hoist? [x local]
    (and (not (or (local x) (core? x))) (or (*env* x) (not (special? x))))) 

  (defn walk-sym [x local]
    (if-not (hoist? x local)
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

  (defn walk-catch [[sym etype bind & body] local]
    (to-list `(~sym ~etype ~bind ~@(map #(walk % (conj local bind)) body))))

  (defn walk-finally [[sym & body] local]
    (to-list `(~sym ~@(map #(walk % local) body))))

  (defn walk-try [[sym & body] local]
    (to-list `(~sym ~@(map #((cond (not (seq? %)) walk
                                   (catch?   %)   walk-catch
                                   (finally? %)   walk-finally
                                   :else          walk)
                             % local)
                           body))))

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
      (to-list `(~sym ~obj ~@(if-not (seq? meth) `[~meth ~@more] [(walk-meth meth)])))))

  (defn walk-list [x local]
    (let [unsupp? #(unsupp? % local)]
      (cond (empty?*   x) x
            (dot?      x) (walk-dot x local)
            (try?      x) (walk-try x local)
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
          (seq?    x) (walk-list x local)
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
      (to-list `((javelin.core/formula ~f) ~@args))))

  (defn set-cell* [c x env]
    (let [[f args] (hoist x env)]
      (list `javelin.core/set-formula! c f args)))

  (defmacro cell=
    ([expr]
     (binding [*cljs* (boolean (:js-globals &env))]
       (cell* expr (or &env {}))))
    ([expr f]
     (binding [*cljs* (boolean (:js-globals &env))]
       (if *cljs*
         `(with-let [c# (cell= ~expr)]
            (set! (.-update c#) ~f))
         `(with-let [c# (cell= ~expr)]
            (clojure.core/dosync (ref-set (.-update c#) ~f)))))))

  (defmacro set-cell!=
    ([c expr] (set-cell* c expr &env))
    ([c expr f]
     (binding [*cljs* (boolean (:js-globals &env))]
       (if *cljs*
         `(with-let [c# c]
            (set-cell!= ~c ~expr)
            (set! (.-update c#) ~f))
         `(with-let [c# c]
            (set-cell!= ~c ~expr)
            (dosync (ref-set (.-update c#) ~f)))))))

  (defmacro defc
    ([sym expr] `(def ~sym (~'javelin.core/cell ~expr)))
    ([sym doc expr] `(def ~sym ~doc (~'javelin.core/cell ~expr))))

  (defmacro defc=
    ([sym expr] `(def ~sym (cell= ~expr)))
    ([sym doc & [expr f]]
       (let [doc? (string? doc)
             f    (when-let [f' (if doc? f expr)] [f'])
             expr (if doc? expr doc)
             doc  (when doc? [doc])]
         `(def ~sym ~@doc (cell= ~expr ~@f)))))

  (defmacro cell-let-1 [[bindings c] & body]
    (let [syms  (bind-syms bindings)
          dcell `((javelin.core/formula (fn [~bindings] [~@syms])) ~c)]
      `(let [[~@syms] (javelin.core/cell-map identity ~dcell)] ~@body)))

  (defmacro cell-let [[bindings c & more] & body]
    (if-not (seq more)
      `(cell-let-1 [~bindings ~c] ~@body)
      `(cell-let-1 [~bindings ~c]
         (cell-let ~(vec more) ~@body))))

  (defmacro dosync [& exprs]
    `(javelin.core/dosync* (fn [] ~@exprs)))

  #_(defmacro cell-doseq [[bindings items] & body]
    `(cell-doseq* ~items (fn [item#] (cell-let [~bindings item#] ~@body))))

  (defmacro cell-doseq [[& seq-exprs] & body]
    (let [pairs (partition 2 seq-exprs)
          lets    (->> pairs (filter (comp (partial = :let) first)) (mapcat second))
          binds*  (->> pairs (take-while (complement (comp keyword? first))))
          mods*   (->> pairs (drop-while (complement (comp keyword? first))) (mapcat identity))
          syms    (->> binds* (mapcat (comp bind-syms first)))
          exprs   (->> binds* (map second))
          gens    (take (count exprs) (repeatedly gensym))
          fors    (-> (->> binds* (map first)) (interleave gens) (concat mods*))]
      `(cell-doseq*
         ((javelin.core/formula (fn [~@gens] (for [~@fors] [~@syms]))) ~@exprs)
         (fn [item#] (cell-let [[~@syms] item#, ~@lets] ~@body)))))

  (defmacro prop-cell
    ([prop]
     (if (:js-globals &env)
       `(let [ret# (cell ~prop)]
          (js/setInterval #(reset! ret# ~prop) 100)
          (cell= ret#))
       (throw (RuntimeException. "prop-cell macro is cljs-only"))))
    ([prop setter & [callback]]
     (if (:js-globals &env)
       `(let [setter#   ~setter
              callback# (or ~callback identity)]
          (cell= (set! ~prop setter#))
          (js/setInterval
           #(when (not= @setter# ~prop)
              (callback# ~prop)
              (set! ~prop @setter#))
           100)
          setter#)
       (throw (RuntimeException. "prop-cell macro is cljs-only"))))))

