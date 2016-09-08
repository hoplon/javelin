(ns javelin.impl-macros
  (:refer-clojure :exclude [dosync set! get]))

(defmacro dosync
  [& body]
  (if (:js-globals &env)
    `(do ~@body)
    `(clojure.core/dosync ~@body)))

(defmacro set!
  [ref-or-place val]
  (if (:js-globals &env)
    `(clojure.core/set! ~ref-or-place ~val)
    `(ref-set ~ref-or-place ~val)))

(defmacro get
  [[fld object]]
  (if (:js-globals &env)
    `(~fld ~object)
    `@(~fld ~object)))
