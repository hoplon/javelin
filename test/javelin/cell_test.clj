;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.cell-test
  (:require
    [clojure.test :as t :refer [deftest testing run-tests is]]
    [javelin.cell :refer :all]))

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
      "swap! or reset! on formula cell returns new value"
      (is (= (swap! a inc) 2))
      (is (= (reset! a 1) 1)))
    (testing
      "side effects are performed correctly"
      (cell= (swap! u conj b))
      (is (= @u [2]))
      (swap! a inc)
      (is (= @u [2 3])))
    (testing
      "attempt to commute cell is an error"
      (is (thrown-with-msg? Exception #"can't commute cell" (dosync (commute a inc)))))
    (testing
      "attempt to swap! or reset! formula cell is an error"
      (is (thrown-with-msg? Exception #"can't alter cell" (swap! b inc)))
      (is (thrown-with-msg? Exception #"can't set cell" (reset! b 3))))))

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

(deftest test-4
  (testing
    "interop and extern functions are lifted correctly:"
    (testing
      "Integer.parseInt()"
      (let [a (cell "123")
            b (cell= (Integer/parseInt a))]
        (is (= @b 123))
        (reset! a "124")
        (is (= @b 124))))
    (testing
      "String.toUpperCase(), String.replace()"
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
            b (cell= (. Math round a))]
        (is (= @b 123))
        (reset! a 41.6)
        (is (= @b 42))))))

(deftest test-5
  (testing
    "without dosync formulas recompute repeatedly"
    (let [u (atom [])
          a (cell 100)
          b (cell 200)
          c (cell= (~(partial swap! u) conj (+ a b)))]
      (do
        (reset! a 150)
        (reset! a 200)
        (reset! b 300))
      (is (= @u [300 350 400 500]))
      (is (= @c [300 350 400 500]))))
  (testing
    "dosync works correctly"
    (let [u (atom {:a [] :b [] :c []})
          a (cell 100)
          b (cell 200)
          c (cell= (+ a b))]
      (add-watch a nil #(swap! u update :a conj %4))
      (add-watch b nil #(swap! u update :b conj %4))
      (add-watch c nil #(swap! u update :c conj %4))
      (dosync
        (reset! a 150)
        (reset! a 200)
        (reset! b 250)
        (reset! b 300))
      (is (= @c 500))
      (is (= @u {:a [200] :b [300] :c [500]}))))
  (testing
    "nested dosyncs are merged correctly"
    (let [u (atom {:a [] :b [] :c []})
          a (cell 100)
          b (cell 200)
          c (cell= (+ a b))
          f #(dosync (reset! a 150) (reset! b 250))
          g #(dosync (swap! a + 50) (swap! b + 50))]
      (add-watch a nil #(swap! u update :a conj %4))
      (add-watch b nil #(swap! u update :b conj %4))
      (add-watch c nil #(swap! u update :c conj %4))
      (dosync (f) (g))
      (is (= @c 500))
      (is (= @u {:a [200] :b [300] :c [500]}))))
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
      (is (= @c '(201 100))))
    (let [a (cell 100)
          b (cell 200)
          c (cell= (when (odd? b) (reset! ~(cell a) (* b 2))))]
      (is (= @a 100))
      (swap! b inc)
      (is (= @a 402)))) 
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
    (let [o (javelin.Mutable. 0)
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
                     [(f1 c) (f2 c)]))]
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
    "unquoted anon fn is not walked"
    (let [a (cell 10)
          b (cell 20)
          c (cell 30)
          d (cell= (mapv ~#(do % (cell? a)) [b c]))]
      (is (= @d [a a]))
      (swap! b inc)
      (is (= @d [a a]))))
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

(deftest misc-tests
  (testing "lenses work correctly using lens function"
    (let [a (cell 100)
          b (cell 200)
          c (cell= (+ a b))
          d (lens c (partial reset! a))]
      (is (not (input? d)))
      (is (= @d 300))
      (reset! d 200)
      (is (= @d 400))
      (swap! d inc)
      (is (= @d 601))))
  (testing "lenses work correctly using cell= macro"
    (let [a (cell 100)
          b (cell 200)
          d (cell= (+ a b) (partial reset! a))]
      (is (not (input? d)))
      (is (= @d 300))
      (reset! d 200)
      (is (= @d 400))
      (swap! d inc)
      (is (= @d 601))))
  (testing "lenses work correctly using defc= macro"
    (let [a (cell 100)
          b (cell 200)]
      (defc= _d (+ a b) (partial reset! a))
      (is (not (input? _d)))
      (is (= @_d 300))
      (reset! _d 200)
      (is (= @_d 400))
      (swap! _d inc)
      (is (= @_d 601))))
  (testing "lens path-cell example works"
    (let [a (cell {:a [1 2 3] :b [4 5 6]})
          path-cell #(cell=
                       (get-in %1 %2)
                       (partial swap! %1 assoc-in %2))
          b (path-cell a [:a])]
      (is (= @b [1 2 3]))
      (is (= (swap! b pop) [1 2]))
      (is (= @b [1 2]))
      (is (= @a {:a [1 2] :b [4 5 6]}))
      (is (= (reset! b :x) :x))
      (is (= @b :x))
      (is (= @a {:a :x :b [4 5 6]}))))
  (testing "swap! or reset! on lens returns new value"
    (let [a (cell 100)
          b (cell 200)
          d (cell= (+ a b) (partial reset! a))]
      (is (= (swap! d inc) 501))
      (is (= (reset! d 100) 300))))
  (testing "lenses propagate correctly"
    (let [a (cell 100)
          b (cell 200)
          d (cell= (+ a b) #(reset! a %))
          e (cell= (+ d a b))]
      (is (= @e 600))
      (reset! d 200)
      (is (= @e 800))))
  (testing "lenses notify watches correctly"
    (let [u (atom [])
          a (cell 100)
          b (cell 200)
          d (cell= (+ a b) #(reset! a %))]
      (add-watch d nil #(swap! u conj {:old %3 :new %4}))
      (reset! d 200)
      (is (= @u [{:old 300 :new 400}]))))
  #_(testing "lenses work correctly in dosync" ;; FIXME: what is correct behavior?
    (let [u  (atom [])
          a  (cell 100)
          b  (cell 200)
          d  (cell= (+ a b) (partial reset! a))
          e  (cell= (swap! u conj d))
          u' (atom [])
          a' (cell 100)
          d' (cell= (+ a' b) (partial reset! a'))
          e' (cell= (swap! u' conj d'))]
      (dosync
        (is (= @d (swap! d inc)))
        (is (= @d (swap! d inc)))
        (is (= @d (swap! d inc))))
      (swap! d' inc)
      (is (= @d 501))
      (is (= @d @d'))
      (is (= @u @u'))))
  (testing "cell-doseq over cell of vector"
    (let [a (cell [1 2 3 4])
          b (atom [])]
      (cell-doseq [x a]
        (swap! b conj (cell= [x (when x (even? x))])))
      (swap! a (partial map inc))
      (let [[p q r s t] @b]
        (is (= 4 (count @b)))
        (is (= @p [2 true]))
        (is (= @q [3 false]))
        (is (= @r [4 true]))
        (is (= @s [5 false]))
        (is (= t nil))
        (testing "cell-doseq growing"
          (swap! a conj 1)
          (let [[p q r s t] @b]
            (is (= 5 (count @b)))
            (is (= @p [1 false]))
            (is (= @q [2 true]))
            (is (= @r [3 false]))
            (is (= @s [4 true]))
            (is (= @t [5 false]))))
        (testing "cell-doseq shrinking"
          (swap! a rest)
          (let [[p q r s t] @b]
            (is (= 5 (count @b)))
            (is (= @p [2 true]))
            (is (= @q [3 false]))
            (is (= @r [4 true]))
            (is (= @s [5 false]))
            (is (= @t [nil nil])))))))
  (testing "cell-doseq over cell of set"
    (let [a (cell [1 2 3 4])
          b (cell= (set a))
          m (atom #{})]
      (is (= @b #{1 2 3 4}))
      (cell-doseq [x b] (swap! m conj x))
      (swap! a (partial map inc))
      (is (= @b #{2 3 4 5}))
      (let [m' (->> @m (map deref) set)]
        (is (= m' #{2 3 4 5})))))
  (testing "cell-doseq, binding to formula expr"
    (let [a (cell [1 2 3 4])
          m (atom [])]
      (cell-doseq [x (cell= (map inc a))]
        (swap! m conj x))
      (swap! a (partial map inc))
      (let [[p q r s] @m]
        (is (= @p 3))
        (is (= @q 4))
        (is (= @r 5))
        (is (= @s 6)))))
  (testing "cell-doseq over cell of seq and destructure"
    (let [a (cell [1 2 3 4])
          b (cell= (map (partial hash-map :x) a))
          m (atom [])]
      (cell-doseq [{:keys [x]} b]
        (swap! m conj x))
      (swap! a (partial map inc))
      (let [[p q r s] @m]
        (is (= @p 2))
        (is (= @q 3))
        (is (= @r 4))
        (is (= @s 5)))))
  (testing "cell-doseq destructure seq item with & more"
    (let [a (cell [[1 2 3] [4 5 6] [7 8 9]])
          m (atom [])]
      (cell-doseq [[x & more] a]
        (swap! m conj [x more]))
      (swap! a (partial map (partial map inc)))
      (let [[[p1 p2] [q1 q2] [r1 r2]] @m]
        (is (= @p1 2))
        (is (= @q1 5))
        (is (= @r1 8))
        (is (= '(3 4) (seq @p2)))
        (is (= '(6 7) (seq @q2)))
        (is (= '(9 10) (seq @r2))))))
  (testing "cell-let correctly binds local cells"
    (let [a (cell [1 2 3 4])
          b (cell {:p 1 :q 2 :r 3 :s 4})]
      (cell-let [[x & xs] a, {:keys [p q r s]} b]
        (is (= @x 1))
        (is (= @xs '(2 3 4)))
        (is (= @p 1))
        (is (= @q 2))
        (is (= @r 3))
        (is (= @s 4))
        (testing "cell-let bindings update when underlying cells change"
          (swap! a (partial map inc))
          (swap! b assoc :p 2 :q 3 :r 4 :s 5)
          (is (= @x 2))
          (is (= @xs '(3 4 5)))
          (is (= @p 2))
          (is (= @q 3))
          (is (= @r 4))
          (is (= @s 5))))))
  (testing "cell-let always binds to cells under destructuring"
    (let [a (cell [1 2])]
      (cell-let [[x y z] a]
        (is (every? cell? [x y z]))
        (is (= @x 1))
        (is (= @y 2))
        (is (= @z nil)))))
  (testing "cell-let binding to formula"
    (let [a (cell [1 2])]
      (cell-let [[x y z] (cell= (map inc a))]
        (is (every? cell? [x y z]))
        (is (= @x 2))
        (is (= @y 3))
        (is (= @z nil))))))

(deftest try-catch-finally
  (testing "try, catch, finally -- body throws"
    (let [a (atom 0)
          b (atom 0)
          c (cell= (try (throw (Exception. "asdf"))
                        (catch Throwable ex
                          (reset! a (.getMessage ex)))
                        (finally (swap! b inc))))]
      (is (= @a "asdf"))
      (is (= @b 1))
      (is (= @c "asdf"))))
  (testing "try, catch, finally -- body doesn't throw"
    (let [a (atom 0)
          b (atom 0)
          c (cell= (try 2
                        (catch Throwable _ (swap! a inc))
                        (finally (swap! b inc))))]
      (is (= @a 0))
      (is (= @b 1))
      (is (= @c 2))))
  (testing "catch, finally are only special in certain cases"
    (let [catch   inc
          finally dec
          a       (atom 0)
          b       (atom 0)
          c       (cell= (try (+ (catch 100) (finally 200)) 
                              (catch Throwable _ (swap! a inc))
                              (finally (swap! b inc))))]
      (is (= @a 0))
      (is (= @b 1))
      (is (= @c 300))))
  #_(testing "constant formula cells propagate constantness" ;; FIXME:
    (let [a 1
          b 2
          c (cell= (+ a b))
          d (cell= (+ a b c))
          e (cell= (+ a b c d))]
      (is (= c (constant? c)))
      (is (= d (constant? d)))
      (is (= e (constant? e)))))
  #_(testing "formula cells that aren't constant don't say they are" ;; FIXME:
    (let [a (cell 1)
          b 2
          c (cell= (+ a b))
          d (cell= (+ a b c))
          e (cell= (+ a b c d))]
      (is (not= c (constant? c)))
      (is (not= d (constant? d)))
      (is (not= e (constant? e)))))

  (testing "fn= returns a formula cell"
    (is (formula? (fn= [] 42))))

  (testing "fn= (formerly formula-of)"
    (let [a (cell 1)
          b (cell 2)
          c (cell 3)
          u (atom 0)
          d (fn= [a b]
              (do (swap! u inc)
                  (+ a b @c)))]
      (testing "initial value is computed correctly"
        (is (= 6 @d))
        (is (= 1 @u)))
      (testing "doesn't update unless declared dependencies change"
        (swap! c inc)
        (is (= 1 @u))
        (is (= 6 @d)))
      (testing "does update when declared dependecies change"
        (swap! a inc)
        (is (= 8 @d))
        (is (= 2 @u)))))

  (testing "let= (formerly formulet)"
    (let [x (cell {:a 1 :b 2})
          y (cell {:c 3 :d 4})
          z (cell {:e 5 :f 6})
          u (atom 0)
          v (let= [{:keys [a b]} x
                   {:keys [c d]} y]
              (do (swap! u inc)
                  (+ a b c d (:e @z) (:f @z))))]
      (testing "initial value is computed correctly"
        (is (= 21 @v))
        (is (= 1 @u)))
      (testing "doesn't update unless declared dependecies change"
        (swap! z update :e inc)
        (is (= 21 @v))
        (is (= 1 @u)))
      (testing "does update when declared dependecies change"
        (swap! x update :a inc)
        (is (= 23 @v))
        (is (= 2 @u))))))

(deftest mutate-cells
  (testing "updating formula does not introduce glitches"
    (let [a (cell 100)
          c (cell= (* 2 a))
          b (cell= (* 2 a))
          d (cell= (/ b c))]
      (is (= [100 200 200 1] [@a @b @c @d]))
      (cell!= c (+ a b))
      (is (= [100 200 300 2/3] [@a @b @c @d]))
      (swap! a inc)
      (is (= [101 202 303 2/3] [@a @b @c @d]))))
  (testing "converting formula cell to input cell works"
    (let [a (cell 100)
          b (cell= (inc a))
          c (cell= (+ a b))
          d (cell= (+ a b c))]
      (is (formula? c))
      (is (= [100 101 201 402] [@a @b @c @d]))
      (cell! c 42)
      (is (input? c))
      (is (= [100 101 42 243] [@a @b @c @d]))
      (swap! a inc)
      (is (= [101 102 42 245] [@a @b @c @d]))
      (swap! c inc)
      (is (= [101 102 43 246] [@a @b @c @d]))))
  (testing "converting input cell to formula cell works"
    (let [a (cell 100)
          b (cell= (inc a))
          c (cell 42)
          d (cell= (+ a b c))]
      (is (input? c))
      (is (= [100 101 42 243] [@a @b @c @d]))
      (cell!= c (+ a b))
      (is (formula? c))
      (is (= [100 101 201 402] [@a @b @c @d]))
      (swap! a inc)
      (is (= [101 102 203 406] [@a @b @c @d])))))

(deftest data-integrity
  ;; Test the data integrity constraints documented in the cells manifesto
  ;;
  ;; http://smuglispweeny.blogspot.com/2008/02/cells-manifesto.html
  ;; 
  (testing "single recomputation of all and only state (a1, a2) affected by change to X"
    (testing "all and only directly dependent state updated"
      (let [x (cell 1)
            y (cell 1)
            a1 (cell= x)
            a2 (cell= (+ x x))
            u (cell= y)
            log (atom [])]
        (add-watch a1 nil (fn [_ _ old new] (swap! log conj [old new])))
        (add-watch a2 nil (fn [_ _ old new] (swap! log conj [old new])))
        ;; log changes to unaffected (u) state
        (add-watch u nil (fn [_ _ old new] (swap! log conj [old new])))

        (swap! x inc)
        (is (= @a1 2))
        (is (= @a2 4))
        (is (= @y 1))
        (is (= (count @log) 2))))

    (testing "indirectly dependent state updated"
      (let [x (cell 1)
            y (cell= (+ x x))
            z (cell= (+ y y))]
        (swap! x inc)
        (is (= @z 8))))

    (testing "recompute exactly once"
      (let [x (cell 1)
            a1 (cell= (+ x x))
            log (atom [])]
        (add-watch a1 nil (fn [_ _ old new] (swap! log conj [old new])))
        (swap! x inc)
        (is (= (count @log) 1))))

    (testing "visibility of change to formula cells"
      (let [x (cell 1)
            b (cell= ["b" x])
            a (cell= [b x])]
        (swap! x inc)
        (is (= @b ["b" 2]))
        (is (= @a [["b" 2] 2]))))

    (testing "visibility of change to observers"
      (let [x (cell 1)
            b (cell= ["b" x])
            a (cell= [b x])
            log (atom [])]
        (add-watch a nil
          (fn [_ _ old new]
            (swap! log conj {:old old :new new})))
        (swap! x inc)
        (is (= @log [{:old [["b" 1] 1] :new [["b" 2] 2]}]))))))
