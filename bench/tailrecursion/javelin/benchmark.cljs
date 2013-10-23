;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin.benchmark
  (:require
    [clojure.string :as s]
    [cemerick.cljs.test :as t]
    [tailrecursion.javelin :refer [cell? input? cell set-cell! alts! destroy-cell!]])
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

(defn run-benchmarks []
  (when (< 0 (apply max ((juxt :fail :error) (run-tests))))
    (throw (js/Error. "some test(s) failed"))))

(defn ^:export start []
  (setup!)
  (time (run-benchmarks)) 
  (println "\nDone."))

;;; benchmarks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-1
  (testing "propagation"
    (let [magic 1000
          expected 768000
          in (cell 0)
          a  (cell= (+ in in))
          b  (cell= (+ a in))
          c  (cell= (+ b a in))
          d  (cell= (+ c b a in))
          e  (cell= (+ d c b a in))
          f  (cell= (+ e d c b a in))
          g  (cell= (+ f e d c b a in))
          h  (cell= (+ g f e d c b a in))
          i  (cell= (+ h g f e d c b a in))
          j  (cell= (+ i h g f e d c b a in))]
      (time (dotimes [_ magic] (swap! in inc)))
      (is (= @j expected)))))
