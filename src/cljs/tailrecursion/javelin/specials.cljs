;   Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin.specials)

(defn no-supported
  [spec-form]
  #(throw (js/Error. (str spec-form " is not supported in cell formulas"))))

(defn if* 
  ([pred consequent] (if pred consequent))
  ([pred consequent alternative] (if pred consequent alternative)))

(def def*         (no-supported "def"))
(def do*          #(last %&))
(def loop**       (no-supported "loop*"))
(def letfn**      (no-supported "letfn*"))
(def throw*       #(if (string? %) (js/Error. %) %))
(def try**        (no-supported "try*"))
(def recur*       (no-supported "recur"))
(def set!*        (no-supported "set!"))
(def ns*          (no-supported "ns"))
(def deftype**    (no-supported "deftype*"))
(def defrecord**  (no-supported "defrecord*"))
(def &*           (no-supported "&"))

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
  ([class a b c d e f g h i & more] (no-supported "new w/more than 10 args")))

