<img src="https://raw.github.com/alandipert/javelin/master/img/javelin.png?login=micha&token=b172f1b97acb55c16867dc106e30c646"
alt="tailrecursion/javelin logo" title="tailrecursion/javelin logo"
align="right" width="152"/>

# Javelin

Spreadsheet-like Functional Reactive Programming (FRP) in ClojureScript.

_This library is usable but under construction and subject to frequent
change._

### Example

```clojure
(ns your-ns
  (:require tailrecursion.javelin) ;; necessary if compiling in advanced mode
  (:require-macros [tailrecursion.javelin.macros :refer [cell cell=]]))

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

```clojure
[tailrecursion/javelin "2.0.0-SNAPSHOT"]
```

### Demos and Examples

For short usage examples, see the [Javelin Demos][3] repository. You may see
some version of these demos running [here][4].

Javelin is also used in two [TodoFRP][5] implementations:

* [Javelin with Domina and Dommy][6]
* [Javelin with Hlisp][7]

## Overview

Javelin provides a spreadsheet-like computing environment consisting
of **cells**, **values**, and **formulas**. Cells are similar to
Clojure atoms: they contain values, they can be dereferenced with
`deref` or the `@` reader macro, and their contents are mutated using
the `swap!` and `reset!` core functions. Formulas are ClojureScript
expressions that may contain references to cells.

### Input Cells and Formula Cells

**Input cells** contain values that are updated explicitly using
`reset!` or `swap!`. **Formula cells** contain values that are
recomputed by Javelin whenever the values in the cells referenced in
the formula expression are changed.

The `cell` macro creates input cells, and the `cell=` macro creates
formula cells.

```clojure
(def a (cell 42))               ;; cell containing the number 42
(def b (cell '(+ 1 2)))         ;; cell containing the list (+ 1 2)
(def c (cell (+ 1 2)))          ;; cell containing the number 3
(def d (cell {:x @a}))          ;; cell containing the map {:x 42}

(def e (cell= {:x a}))          ;; cell with formula {:x a}, updated when a changes
(def f (cell= (+ a 1)))         ;; cell with formula (+ a 1), updated when a changes
(def g (cell= (+ a ~(inc @a)))) ;; cell with formula (+ a 43), updated when a changes

(cell= (.log js/console a))     ;; value of a is printed whenever it's updated

(reset! a 7)                    ;; ok, because a is an input cell
(swap! f inc)                   ;; no! f is a formula cell, it updates itself!
```

### Cell Macro Internals

The `cell` and `cell=` macros create cells using the underlying
`input` and `lift` functions. The former returns an input cell with
the given initial value. The latter "lifts" a given function,
returning a function that, when applied to arguments (which may be
cells) returns a cell with the given function as the formula
&mdash; this cell's value is recomputed whenever any of the
argument cells change.

```clojure
(def x (input 7))       ;; input cell with initial value 7
(def y ((lift +) x 1))  ;; equivalent to (def y (cell= (+ x 1)))
```

To create a formula cell, the `cell=` macro fully macroexpands the
given expression and then walks it, recursively lifting all forms in
function position. However, there are several special cases and
exceptions:

* **Special forms** are replaced with equivalent function
  implementations.
* But some special forms **can not be lifted**: `def`, `loop*`, `letfn*`,
  `try*`, `recur`, `ns`, `deftype*`, `defrecord*`, and `&`.
* **Collection literals** are replaced with their sexp equivalents
  and then walked.
* **Anonymous function bodies** are not walked.
* **Quoted expressions** are not walked.
* **The unquote form** causes its argument to be evaluated in place
  and not walked.
* **The unquote-splicing form** is interpreted as the composition
  of `unquote` and `deref`.

### Issues With Special Forms

The spreadsheet evaluation model is a push-based system, very
different from the usual, pull-based Lisp evaluation model. In Lisp,
forms are evaluated depth first, and only as needed to produce a
value. This model supports special forms and macros, which decide when
to evaluate their own arguments. In the Javelin evaluation model this
is impossible because formula cells are re-computed _reactively_ based
on the values of the argument cells (cells the formula cell depends
on), which must therefore be computed first.

This causes the behavior of "short-circuiting" expressions like `and`
and `if` to be strange. If the short-circuiting behavior is required
then the expression must be wrapped in an anonymous function.

Also, some forms (like `for` or `doseq`, for example) macroexpand to
expressions based on unsupported special forms like `loop*`. Expressions
using these forms must also be wrapped in anonymous functions.

```clojure
(def x (cell 1))
(def y (cell 1))
(def z (cell [1 2 3]))

;; This cell prints both "even" and "odd".
(cell= (if (even? (+ x y)) (.log js/console "even") (.log js/console "odd")))

;; This cell only prints "even" or "odd".
(cell= (#(if (even? (+ %1 %2)) (.log js/console "even") (.log js/console "odd")) x y))

;; This throws a js error because loop* is not supported.
(cell= (doseq [i z] (.log js/console i)))

;; This works as intended because the cell= macro doesn't walk the fn.
(cell= (#(doseq [i %] (.log js/console i)) z))
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
[3]: https://github.com/tailrecursion/javelin-demos
[4]: https://dl.dropboxusercontent.com/u/12379861/javelin_demos/index.html
[5]: https://github.com/lynaghk/todoFRP
[6]: https://github.com/lynaghk/todoFRP/tree/master/todo/javelin
[7]: https://github.com/lynaghk/todoFRP/tree/master/todo/hlisp-javelin
