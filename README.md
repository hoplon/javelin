<img src="https://raw.github.com/tailrecursion/javelin/master/img/javelin.png"
alt="tailrecursion/javelin logo" title="tailrecursion/javelin logo"
align="right" width="152"/>

# Javelin

Spreadsheet-like dataflow programming in ClojureScript.

### Example

```clojure
(ns your-ns
  (:require [tailrecursion.javelin :refer [cell]])
  (:require-macros [tailrecursion.javelin :refer [cell=]]))

(defn start []
  (let [a (cell 0)              ;; input cell with initial value of 0.
        b (cell= (inc a))       ;; formula cell of a+1.
        c (cell= (+ 123 a b))]  ;; formula cell of a+b+123.
    (cell= (.log js/console c)) ;; anonymous formula cell for side effects.
    ;; c's initial value, 124, is printed.
    (swap! a inc)
    ;; a was incremented, and its new value propagated (consistently)
    ;; through b and c.  c's new value, 126, is printed to the console.
    ))
```

### Dependency [![Build Status][1]][2]

Artifacts are published on [Clojars][3].

![latest version][10]

### Demos

* [Javelin with Domina and Dommy][7]
* [Javelin with Hoplon][8]

## Overview

Javelin provides a spreadsheet-like computing environment consisting
of **input cells** and **formula cells** and introduces the `Cell`
reference type to represent both.

##### All Cells

* contain values.
* implement the `IWatchable` interface.
* are dereferenced with `deref` or the `@` reader macro. 

##### Input Cells

* are created by the `cell` function or `defc` macro.
* are updated explicitly using `swap!` or `reset!`.

##### Formula Cells

* are created by the `cell=` or `defc=` macros.
* are updated _reactively_ according to a formula.
* are read-only&mdash;updating a formula cell via `swap!` or `reset!`
  is an error.

Some examples of cells:

```clojure
(defc a 42)               ;; cell containing the number 42
(defc b '(+ 1 2))         ;; cell containing the list (+ 1 2)
(defc c (+ 1 2))          ;; cell containing the number 3
(defc d {:x @a})          ;; cell containing the map {:x 42}

(defc= e {:x a})          ;; cell with formula {:x a}, updated when a changes
(defc= f (+ a 1))         ;; cell with formula (+ a 1), updated when a changes
(defc= g (+ a ~(inc @a))) ;; cell with formula (+ a 43), updated when a changes
(defc= h [e f g])         ;; cell with formula [e f g], updated when e, f, or g change

@h                        ;;=> [{:x 42} 43 85]
(reset! a 7)              ;;=> 7
@h                        ;;=> [{:x 7} 8 50]
(swap! f inc)             ;;=> ERROR: f is a formula cell, it updates itself!
```

Note the use of `~` in the definition of `g`. The expression
`(inc @a)` is evaluated and the resulting value is used when creating
the formula, rather than being recomputed each time the cell updates.
See the [Formulas][9] section below.

Cells can be microbeasts...

```clojure
(defc test-results
  {:scores [74 51 97 88 89 91 72 77 69 72 45 63]
   :proctor "Mr. Smith"
   :subject "Organic Chemistry"
   :sequence "CHM2049"})

(defc= test-results-with-mean
  (let [scores (:scores test-results)
        mean   (/ (reduce + scores) (count scores))
        grade  (cond (<= 90 mean) :A
                     (<= 80 mean) :B
                     (<= 70 mean) :C
                     (<= 60 mean) :D
                     :else        :F)]
    (assoc test-results :mean mean :grade grade)))
```

## Formulas

All macros in formula expressions are fully expanded. The resulting
expression is then interpreted according to the following rules:

* **The unquote form** causes its argument to be evaluated in place
  and not walked.
* **The unquote-splicing form** is interpreted as the composition
  of `unquote` as above and `deref`.

Some things don't make sense in formulas and cause errors:

* **Unsupported forms** `def`, `ns`, `deftype*`, and `defrecord*`.
* **Circular dependencies** between cells result in infinite loops at
  runtime.

## Javelin API

Requiring the namespace and macros:

```clojure
(ns my-ns
  (:require
    [tailrecursion.javelin
     :refer [cell? input? cell set-cell! alts! destroy-cell! cell-map]])
  (:require-macros
    [tailrecursion.javelin
     :refer [cell= defc defc= set-cell!= cell-doseq]]))
```

API functions and macros:

```clojure
(cell? c)
;; Returns c if c is a Cell, nil otherwise.

(input? c)
;; Returns c if c is an input cell, nil otherwise.

(cell expr)
;; Create new input cell with initial value expr.

(cell= expr)
;; Create new fomula cell with formula expr.

(defc symbol doc-string? expr)
;; Creates a new input cell and binds it to a var with the name symbol and
;; the docstring doc-string if provided.

(defc= symbol doc-string? expr)
;; Creates a new formula cell and binds it to a var with the name symbol and
;; the docstring doc-string if provided.

(set-cell! c expr)
;; Convert c to input cell (if necessary) with value expr.

(set-cell!= c expr)
;; Convert c to formula cell (if necessary) with formula expr.

(destroy-cell! c)
;; Disconnects c from the propagation graph so it can be GC'd.

(alts! cs*)
;; Creates a formula cell whose value is a list of changed values in the cells cs.

(cell-map f c)
;; Given a cell c containing a seqable value of size n and a function f, returns
;; a sequence of n formula cells such that the ith cell's formula is (f (nth c i)).

(cell-doseq seq-expr body*)
;; Repeatedly executes the body expression(s) for side effects as doseq does.
;; However seq-expr is a single binding-form/collection-cell-expr pair instead
;; of the multiple pairs allowed in doseq, and binding forms are bound to formula
;; cells containing the destructured values which will update as the collection
;; expr cell is changed.
```

## License

    Copyright (c) Alan Dipert and Micha Niskin. All rights
    reserved. The use and distribution terms for this software are
    covered by the Eclipse Public License 1.0
    (http://opensource.org/licenses/eclipse-1.0.php) which can be
    found in the file epl-v10.html at the root of this
    distribution. By using this software in any fashion, you are
    agreeing to be bound by the terms of this license. You must not
    remove this notice, or any other, from this software.

[1]: https://travis-ci.org/tailrecursion/javelin.png?branch=master
[2]: https://travis-ci.org/tailrecursion/javelin
[3]: http://clojars.org/tailrecursion/javelin
[4]: https://github.com/tailrecursion/javelin-demos
[5]: https://dl.dropboxusercontent.com/u/12379861/javelin_demos/index.html
[6]: https://github.com/lynaghk/todoFRP
[7]: https://github.com/lynaghk/todoFRP/tree/master/todo/javelin
[8]: https://github.com/tailrecursion/hoplon-demos/tree/master/todoFRP
[9]: https://github.com/tailrecursion/javelin#formulas
[10]: http://clojars.org/tailrecursion/javelin/latest-version.svg
[11]: tree/master/img/javelin.png
