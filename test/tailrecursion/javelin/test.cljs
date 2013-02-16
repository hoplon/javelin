;   Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin.test
  (:require [tailrecursion.javelin :refer [all! distinct!]])
  (:require-macros [tailrecursion.javelin.macros :refer [mx cell are=]]))

(defn ^:export start []

  (let [a (cell 42)
        b (cell '(+ 1 2))
        c (cell (+ a 1))]
    (are= 42 @a, 3 @b, 43 @c))

  (let [a (cell 0)
        b (cell (inc a))
        c (cell (+ 123 a b))]
    (are= 0 @a, 1 @b, 124 @c)
    (swap! a inc)
    (are= 1 @a, 2 @b, 126 @c))

  (let [a (cell 0)
        b (cell (conj ~[] a))]
    (are= 0 @a, [0] @b)
    (swap! a inc)
    (swap! a inc)
    (are= 2 @a, [0 1 2] @b))

  (let [a (cell "123")
        b (cell (js/parseInt a))]
    (are= 123 @b))

  (let [a (cell "abc")
        b (cell "def")
        c (cell (.toUpperCase b))
        d (cell (str a c))]
    (are= "abcDEF" @d)
    (swap! b #(.replace % "d" "z"))
    (are= "abcZEF" @d))

  (let [a (cell 123.2)
        b (cell (.round js/Math a))]
    (are= 123 @b))

  (let [a (cell 15)
        b (cell (+ 4 a))
        c (cell (/ b 2))
        d (cell (* b c))]
    (swap! a inc)
    (are= 200 @d))

  (let [a (all! (cell 0))
        b (cell (conj ~[] a))]
    (swap! a identity)
    (swap! a identity)
    (are= 0 @a, [0 0 0] @b)
    (distinct! a)
    (swap! a identity)
    (are= 0 @a, [0 0 0] @b)))

(.log js/console "__exit__")

(comment
  (.log js/console (pr-str (mx (cell (.toUpperCase a))))))
