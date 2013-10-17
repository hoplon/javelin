;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin.test
  (:require
    [clojure.string :as s]
    [cemerick.cljs.test :as t]
    [tailrecursion.javelin :refer [cell? input? cell set-cell! destroy-cell!]])
  (:require-macros
    [cemerick.cljs.test :refer [deftest testing run-tests is]]
    [tailrecursion.javelin :refer [cell= defc defc= set-cell!= mx mx2]]))

;;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn setup! []
  (set! cljs.core/*print-fn*
        (if (undefined? (aget js/window "dump"))
          ;; phantomjs
          (fn [arg]
            (let [arg (s/trim-newline (str arg))]
              (.call (.-log js/console) (.-console js/window) arg)))
          ;; firefox
          (fn [arg]
            (.call (aget js/window "dump") js/window arg)))))

(defn run-tests* []
  (when (< 0 (apply max ((juxt :fail :error) (run-tests))))
    (throw (js/Error. "some test(s) failed"))))

;(defn ^:export start []
;  (setup!)
;  (defc a 42.3)
;  (mx2 #(+ % a))
;  (mx2 (fn [x] (+ x a)))
;  ) 

(defn ^:export start []
  (setup!)
  (time (run-tests*)) 
  (println "\nDone."))

;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-1
  (let [a (cell 0)
        b (cell= (inc a))
        u (atom [])]
    (defc p 0)
    (defc= q (inc a))
    (testing
      "initial values are correct"
      (is (= @a 0))
      (is (= @b 1)))
    (testing
      "defc and defc= perform correctly"
      (is (= @a @p))
      (is (= @b @q)))
    (testing
      "printed representation is correct"
      (is (= (pr-str a) "#<Cell: 0>"))
      (is (= (pr-str b) "#<Cell: 1>")))
    (testing
      "cell? correctly identifies cells"
      (is (= a (cell? a)))
      (is (= b (cell? b)))
      (is (= nil (cell? u))))
    (testing
      "input? correctly identifies input cells"
      (is (= a (input? a)))
      (is (= nil (input? b)))
      (is (= nil (input? u))))
    (testing
      "simple formula updates correctly"
      (swap! a inc)
      (is (= @a 1))
      (is (= @b 2)))
    (testing
      "side effects are performed correctly"
      (cell= (swap! u conj b))
      (is (= @u [2]))
      (swap! a inc)
      (is (= @u [2 3])))
    (testing
      "attempt to swap! or reset! formula cell is an error"
      (is (thrown-with-msg? js/Error #"can't be updated" (swap! b inc)))
      (is (thrown-with-msg? js/Error #"can't be updated" (reset! b 3))))))

(deftest test-2
  (let [a (cell 0)
        b (cell= (inc a))
        u (atom [])
        v (atom [])]
    (testing
      "watches are added correctly"
      (add-watch a nil (fn [_ _ x y] (swap! u conj [x y])))
      (add-watch b nil (fn [_ _ x y] (swap! v conj [x y])))
      (swap! a inc)
      (is (= @u [[0 1]]))
      (is (= @v [[1 2]])))
    (testing
      "watches are removed correctly"
      (remove-watch b nil)
      (swap! a inc)
      (is (= @u [[0 1] [1 2]]))
      (is (= @v [[1 2]])))))

(deftest test-3
  (let [u (atom [])
        a (cell 0)
        b (cell= (do (swap! u conj a) (inc a)))
        c (cell= (+ 123 a b))]
    (testing
      "change formula of formula cell"
      (swap! a inc)
      (set-cell!= b (dec a))
      (swap! a inc)
      (is (= @a 2))
      (is (= @b 1))
      (is (= @c 126)))
    (testing
      "anonymous cells in formula are destroyed correctly"
      (is (= @u [0 1])))
    (testing
      "convert formula cell to input cell"
      (set-cell! c :hello)
      (is (= c (input? c)))
      (is (= @c :hello))
      (reset! c :goodbye))
    (testing
      "convert input cell to formula cell"
      (set-cell!= a (count (str c)))
      (is (= nil (input? a)))
      (is (= @a 8))
      (is (= @b 7))
      (reset! c :hello)
      (is (= @a 6))
      (is (= @b 5)))))

(deftest test-4
  (testing
    "interop and extern functions are lifted correctly:"
    (testing
      "window.parseInt()"
      (let [a (cell "123")
            b (cell= (js/parseInt a))]
        (is (= @b 123))
        (reset! a "124")
        (is (= @b 124))))
    (testing
      "String.toUpperCase()"
      (let [a (cell "abc")
            b (cell "def")
            c (cell= (.toUpperCase b))
            d (cell= (str a c))]
        (is (= @d "abcDEF"))
        (swap! b #(.replace % "d" "z"))
        (is (= @d "abcZEF")))) 
    (testing
      "Math.round()"
      (let [a (cell 123.2)
            b (cell= (.round js/Math a))]
        (is (= @b 123))
        (reset! a 41.6)
        (is (= @b 42))))))

(deftest test-5
  (testing
    "quoted expressions in formulas are not walked"
    (let [a (cell 100)
          b (cell= (cons a '(a)))]
      (is (= @b '(100 a)))
      (swap! a inc)
      (is (= @b '(101 a)))))
  (testing
    "unquote (~) expressions in formulas are not walked"
    (let [a (cell 100)
          b (cell 200)
          c (cell= (cons b ~(list @a)))]
      (is (= @c '(200 100)))
      (swap! a inc)
      (is (= @c '(200 100)))
      (swap! b inc)
      (is (= @c '(201 100)))))
  (testing
    "unquote-splicing (~@) in formulas is equiv to ~(deref expr)"
    (let [a (cell 100)
          b (cell 200)
          c (cell= (+ ~@a b))]
      (is (= @c 300))
      (swap! a inc)
      (is (= @c 300))
      (swap! b inc)
      (is (= @c 301))))
  (testing
    "chains of formulas perform correctly"
    (let [a (cell 15)
          b (cell= (+ 4 a))
          c (cell= (/ b 2))
          d (cell= (* b c))]
      (swap! a inc)
      (is (= 200 @d)))) 
  (testing
    "complicated formulas are ok"
    (let [a (cell= ((comp inc (comp inc identity)) 123))]
      (is (= 125 @a)))) 
  (testing
    "higher order functions in formulas work correctly"
    (let [a (cell [1 2 3])
          b (cell= (mapv (fn [x] (inc x)) a))]
      (is (= [2 3 4] @b)))) 
  (testing
    "map formulas perform correctly"
    (let [a (cell 0)
          b (cell= {:a a})]
      (is (= {:a 0} @b)) 
      (swap! a inc)
      (is (= {:a 1} @b)))) 
  (testing
    "map input cells perform correctly"
    (let [a (cell {:n 0})]
      (is (= 0 (:n @a))) 
      (swap! a update-in [:n] inc)
      (is (= 1 (:n @a))))) 
  (testing
    "cells in function position are handled correctly"
    (let [a (cell 0)
          b (cell (fn [x] (inc x)))
          c (cell= (b a))]
      (is (= 1 @c)) 
      (reset! b (fn [x] (dec x)))
      (is (= -1 @c)))) 
  (testing
    "side effects are performed correctly"
    (let [effect1 (atom nil)
          effect2 (atom nil)
          a (cell 1)
          b (cell 1)
          c (cell 1)
          d (cell= (let [x (+ a b)]
                     (reset! effect1 x)
                     (reset! effect2 c)
                     (* x 2)))]
      (is (= 4 @d)) 
      (is (= 2 @effect1)) 
      (is (= 1 @effect2)) 
      (swap! a inc)
      (swap! c inc)
      (is (= 6 @d))
      (is (= 3 @effect1))
      (is (= 2 @effect2)))) 
  (testing
    "map lookups are handled correctly"
    (let [m (cell {:some-kw [1 2 3]})
          a (cell= (seq (:some-kw m)))
          b (cell= (reduce + a))]
      (is (= 6 @b))) 
    (let [m (cell {:some-kw [1 2 3]})
          a (cell= {:sum (reduce + (:some-kw m))})]
      (is (= 6 (:sum @a))))) 
  (testing
    "booleans are handled correctly"
    (let [a (cell true)
          b (cell false)
          c (cell= (not (or a b)))]
      (is (= false @c)))) 
  (testing
    "set! is handled correctly"
    (let [o (js-obj)
          a (cell 0)]
      (cell= (set! (.-foo o) (inc a)))
      (is (= (.-foo o) 1)) 
      (swap! a inc)
      (is (= (.-foo o) 2)))))

(deftest test-6
  (testing
    "letfn works correctly"
    (let [a (cell 2)
          b (cell 3)
          c (cell 10)
          d (cell= (letfn [(f1 [x] (* a (f2 x)))
                           (f2 [x] (* b x))]
                     [(f1 c) (f2 c)]))
          
          ]
      (is (= @d [60 30]))
      (swap! a * 2)
      (is (= @d [120 30]))))
  (testing
    "doseq works correctly"
    (let [u (atom [])
          a (cell [1 2 3 4])
          b (cell= (doseq [x (filter odd? a)] (swap! u conj x)))]
      (is (= @u [1 3]))
      (swap! a conj 5)
      (is (= @u [1 3 1 3 5]))))
  (testing
    "short-circuiting forms do the right thing"
    (let [u (atom [])
          a (cell 1)
          b (cell 2)
          c (cell= (if (odd? (+ a b))
                     (do (swap! u conj "odd") "odd")
                     (do (swap! u conj "even") "even")))]
      (is (= @c "odd"))
      (is (= @u ["odd"]))
      (swap! a inc)
      (is (= @c "even"))
      (is (= @u ["odd" "even"]))))
  (testing
    "anon fn via #() reader macro"
    (let [a (cell 10)
          b (cell 20)
          c (cell= #(+ a %))
          d (cell= (c b))]
      (is (= @d 30))
      (swap! a / 2)
      (is (= @d 25))))
  (testing
    "anon fn with name binds name locally"
    (let [a (cell 10)
          b (cell 20)
          c (cell= (fn asdf [x] (if (even? x) (+ x (asdf (/ x 2))) (+ x a)))) 
          d (cell= (c b))]
      (is (= @d 45))
      (swap! a / 2)
      (is (= @d 40))))
  (testing
    "anon fn with destructuring arglist works correctly"
    (let [a (cell 10)
          b (cell 20)
          c (cell= (fn asdf [& args] (mapv inc args)))
          d (cell= (c a b))]
      (is (= @d [11 21]))
      (swap! a / 2)
      (is (= @d [6 21]))))
  (testing
    "anon fn with destructuring arglist works correctly part 2"
    (let [a (cell 10)
          b (cell 20)
          c (cell= (fn asdf [x & {:keys [y z] :or {y 0 z 0}}]
                     (mapv inc [x y z])))
          d (cell= (c a :y b))]
      (is (= @d [11 21 1]))
      (swap! a / 2)
      (is (= @d [6 21 1]))))
  (testing
    "case works correctly"
    (let [a (cell 1)
          b (cell= (case a 1 :1 2 :2 :oops))]
      (is (= @b :1))
      (swap! a inc)
      (is (= @b :2))
      (swap! a inc)
      (is (= @b :oops))))
  (testing
    "cond works correctly"
    (let [a (cell 1)
          b (cell= (cond (= a 1) :1 (= a 2) :2 :else :oops))]
      (is (= @b :1))
      (swap! a inc)
      (is (= @b :2))
      (swap! a inc)
      (is (= @b :oops)))))
