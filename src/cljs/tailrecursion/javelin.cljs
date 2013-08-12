;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.javelin
  (:require-macros
   [tailrecursion.javelin.macros :refer [with-let cell mirror]])
  (:require
   [tailrecursion.javelin.core :as core]
   tailrecursion.javelin.specials))

;; internal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mirror tailrecursion.javelin.core)

(mirror tailrecursion.javelin.specials)

;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def log        #(js/console.log %1 (clj->js %2)))
(def timeout    #(.setTimeout js/window %1 %2))
(def interval   #(.setInterval js/window %1 %2))

(defn route*
  [msec default]
  (let [hash  #(.-hash (.-location js/window))
        ret   (cell '(hash))]
    (interval #(let [h (hash)] (reset! ret (if (empty? h) default h))) msec)
    ret))

(defn timer*
  [msec f init]
  (with-let [out (cell init)]
    ((fn rec []
       (timeout #(do (if-not (neg? @msec) (swap! out f)) (rec))
                (if (neg? @msec) 0 @msec))))))
