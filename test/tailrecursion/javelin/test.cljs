;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin.test
  (:require tailrecursion.javelin)
  (:require-macros
    [tailrecursion.javelin.macros
     :refer [cell? cell cell= set-cell! set-cell!= mx are=]]))

(defn setup! []
  (set! cljs.core/*print-fn*
        (if (undefined? (aget js/window "dump"))
          ;; phantomjs
          (fn [& args]
            (.apply (.-log js/console)
                    (.-console js/window)
                    (apply array args)))
          ;; firefox
          (fn [& args]
            (.apply (aget js/window "dump")
                    js/window
                    (apply array args))))))

(defn ^:export start []

  (setup!)

  (time
   (do
     (let [a (cell 42)
           b (cell (+ 1 2))
           c (cell= (+ a 1))]
       (are= "#<Cell: 42>" (pr-str a))
       (are= true   (cell? a)
             true   (cell? b)
             true   (cell? c)
             false  (cell? (atom 0)))
       (are= 42 @a
             3  @b
             43 @c))

     (let [a (try
               (let [a (cell 42)
                     b (cell= (inc a))]
                 (reset! b 1337))
               (catch js/Error e :exception-thrown))]
       (are= a :exception-thrown)) 

     (let [x (atom [])
           a (cell 0)
           b (cell= (do (swap! x conj a) (inc a)))
           c (cell= (+ 123 a b))]
       (are= 0   @a
             1   @b
             124 @c)
       (swap! a inc)
       (are= 1   @a
             2   @b
             126 @c)
       (set-cell!= b (dec a))
       (are= 1   @a
             0   @b
             124 @c)
       (swap! a inc)
       (are= 2   @a
             1   @b
             126 @c)
       (set-cell! b 10)
       (are= 2   @a
             10  @b
             135 @c)
       (swap! b inc)
       (are= 2   @a
             11  @b
             136 @c)
       (are= [0 1] @x))

     (let [a (cell "123")
           b (cell= (js/parseInt a))]
       (are= 123 @b))

     (let [a (cell "abc")
           b (cell "def")
           c (cell= (.toUpperCase b))
           d (cell= (str a c))]
       (are= "abcDEF" @d)
       (swap! b #(.replace % "d" "z"))
       (are= "abcZEF" @d))

     (let [a (cell 123.2)
           b (cell= (.round js/Math a))]
       (are= 123 @b))

     (let [a (cell 15)
           b (cell= (+ 4 a))
           c (cell= (/ b 2))
           d (cell= (* b c))]
       (swap! a inc)
       (are= 200 @d))

     (let [a (cell= ((comp inc (comp inc identity)) 123))]
       (are= 125 @a))

     (let [a (cell [1 2 3])
           b (cell= (mapv (fn [x] (inc x)) a))]
       (are= [2 3 4] @b))

     (let [a (cell 0)
           b (cell= {:a a})]
       (are= {:a 0} @b)
       (swap! a inc)
       (are= {:a 1} @b))

     (let [a (cell {:n 0})]
       (are= 0 (:n @a))
       (swap! a update-in [:n] inc)
       (are= 1 (:n @a)))

     (let [a (cell 0)
           b (cell (fn [x] (inc x)))
           c (cell= (b a))]
       (are= 1 @c)
       (reset! b (fn [x] (dec x)))
       (are= -1 @c))

     (let [effect1 (atom nil)
           effect2 (atom nil)
           a (cell 1)
           b (cell 1)
           c (cell 1)
           d (cell= (let [x (+ a b)]
                      (reset! effect1 x)
                      (reset! effect2 c)
                      (* x 2)))]
       (are= 4 @d
             2 @effect1
             1 @effect2)
       (swap! a inc)
       (swap! c inc)
       (are= 6 @d
             3 @effect1
             2 @effect2))

     (let [m (cell {:some-kw [1 2 3]})
           a (cell= (seq (:some-kw m)))
           b (cell= (reduce + a))]
       (are= 6 @b))

     (let [m (cell {:some-kw [1 2 3]})
           a (cell= {:sum (reduce + (:some-kw m))})]
       (are= 6 (:sum @a)))

     (let [a (cell true)
           b (cell false)
           c (cell= (not (or a b)))]
       (are= false @c))

     (let [in (cell 0)]
       (cell=
        (+
         (+
          (+
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))))
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))))
          (+
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))))
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))))))
         (+
          (+
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))))
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))))
          (+
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))))
           (+
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))))
            (+
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))
             (+
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in))))
              (+
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))
               (+ (+ (+ in) (+ in)) (+ (+ in) (+ in)))))))))))

       (dotimes [_ 10]
         (time (dotimes [_ 10] (swap! in inc))))

       (println "total"))

     (let [o (js-obj)
           a (cell 0)]
       (cell= (set! (.-foo o) (inc a)))
       (are= (.-foo o) 1)
       (swap! a inc)
       (are= (.-foo o) 2))))

  (println "Done."))
