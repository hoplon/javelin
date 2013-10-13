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

Artifacts are published on [Clojars][3].

```clojure
[tailrecursion/javelin "2.0.0-SNAPSHOT"]
```

```xml
<dependency>
  <groupId>tailrecursion</groupId>
  <artifactId>javelin</artifactId>
  <version>2.0.0-SNAPSHOT</version>
</dependency>
```

### Demos and Examples

For short usage examples, see the [Javelin Demos][4] repository. You may see
some version of these demos running [here][5].

Javelin is also used in two [TodoFRP][6] implementations:

* [Javelin with Domina and Dommy][7]
* [Javelin with Hlisp][8]

## Overview

Javelin provides a spreadsheet-like computing environment consisting
of **cells**, **values**, and **formulas**. Cells are similar to
Clojure atoms: they contain values and they can be dereferenced with
`deref` or the `@` reader macro. Formulas are ClojureScript expressions
that may contain references to other cells.

**Input cells**
* contain values that are updated explicitly using `reset!` or `swap!`.
* implement `IWatchable` so `add-watch` and `remove-watch` can be used.
* are created by the `cell` macro.

**Formula Cells**
* contain values that are recomputed _reactively_ according to a formula.
* are read-only&mdash;attempts to update a formula cell directly
  via `swap!` or `reset!` results in an error.
* do not implement `IWatchable`&mdash;use anonymous cells instead.
* are created by the `cell=` macro.

Some examples of cells:

```clojure
(def a (cell 42))               ;; cell containing the number 42
(def b (cell '(+ 1 2)))         ;; cell containing the list (+ 1 2)
(def c (cell (+ 1 2)))          ;; cell containing the number 3
(def d (cell {:x @a}))          ;; cell containing the map {:x 42}

(def e (cell= {:x a}))          ;; cell with formula {:x a}, updated when a changes
(def f (cell= (+ a 1)))         ;; cell with formula (+ a 1), updated when a changes
(def g (cell= (+ a ~(inc @a)))) ;; cell with formula (+ a 43), updated when a changes
(def h (cell= [e f g]))         ;; cell with formula [e f g], updated when e, f,
                                ;; and/or g change

@h                              ;;=> [{:x 42} 43 85]
(reset! a 7)                    ;;=> 7
@h                              ;;=> [{:x 7} 8 50]
(swap! f inc)                   ;;=> ERROR: f is a formula cell, it updates itself!
```

Note the use of `~` in the definition of `g`. The expression
`(inc @a)` is evaluated and the resulting value is used when creating
the formula, rather than being recomputed each time the cell updates.
See the [Formulas][9] section below.

### Formulas

The `cell` and `cell=` macros create cells using the underlying
`input` and `lift` functions. The former returns an input cell with
the given initial value. The latter "lifts" a given function,
returning a function that, when applied to arguments (which may be
cells) returns a cell with the given function as the formula
&mdash; this cell's value is recomputed whenever any of the
argument cells change.

```clojure
(def x (input 7))       ;; similar to (def x (cell 7))
(def y ((lift +) x 1))  ;; similar to (def y (cell= (+ x 1)))
```

To create a formula cell: first, all macros in the given formula
expression are fully expanded. The resulting form is then walked
recursively and all forms in function position are lifted, subject
to the following special cases and exceptions:

* **Special forms** `if`, `do`, `new`, and `throw` are replaced during
  the code-walking phase with equivalent reactive implementations.
* **Special forms** `def`, `loop*`, `letfn*`, `try*`, `recur`, `ns`,
  `deftype*`, `defrecord*`, and `&` are not supported and cause a
  runtime exception to be thrown.
* **Collection literals** are replaced with their sexp equivalents
  and then walked.
* **Anonymous function bodies** are not walked.
* **Quoted expressions** are not walked.
* **The unquote form** causes its argument to be evaluated in place
  and not walked.
* **The unquote-splicing form** is interpreted as the composition
  of `unquote` and `deref`.
* **Circular references** result in an infinite loop and stack overflow.

#### Special Forms In Formulas

The spreadsheet evaluation model is a push-based system, very
different from the usual, pull-based Lisp evaluation model. In Lisp,
forms are evaluated depth first, and only as needed to produce a
value. This model supports special forms and macros, which decide when
to evaluate their own arguments. In the Javelin evaluation model this
is impossible because formula cells are re-computed _reactively_ based
on the values of the argument cells (cells the formula cell depends
on), which must therefore be computed first.

Consequences of this include:
* "Short-circuiting" expressions (like `and` and `if`, for example)
  don't work that way when used in a formula&mdash;all clauses are
  always evaluated. The cell's value will be correct (i.e. the cell
  will contain the value of the correct clause) but side effects in
  all clauses will be performed on every update.
* Macros that expand to expressions containing unsupported special
  forms (like `doseq` and `for`, for example, which expand to
  expressions containing the unsupported `loop*` form) can't be
  used in formulas.

In these cases the solution is to wrap the expression in an anonymous
function to protect it from being lifted when the `cell=` macro walks
the code. The `cell=` macro will not descend into anonymous function
bodies.

For example:

```clojure
(def x (cell 1))
(def y (cell 1))
(def z (cell [1 2 3]))

;; This cell prints both "even" and "odd".
(cell= (if (even? (+ x y)) (.log js/console "even") (.log js/console "odd")))

;; This cell prints only "even" or "odd".
(cell= (#(if (even? (+ %1 %2)) (.log js/console "even") (.log js/console "odd")) x y))

;; This throws a js error because loop* is not supported.
(cell= (doseq [i z] (.log js/console i)))

;; This works as intended because the cell= macro doesn't walk the fn.
(cell= (#(doseq [i %] (.log js/console i)) z))
```

### Javelin Cell API

Requiring the namespace and macros:

```clojure
(ns my-ns
  (:require tailrecursion.javelin)
  (:require-macros
    [tailrecursion.javelin.macros
     :refer [cell? cell cell= set-cell! set-cell!= destroy-cell!]]))
```

Cell macros:

```clojure
(cell? c)
;; Returns true if c is a Cell, false otherwise.

(cell expr)
;; Create new input cell with initial value expr.

(cell= expr)
;; Create new fomula cell with formula expr.

(set-cell! c expr)
;; Convert c to input cell (if necessary) with initial value expr.

(set-cell!= c expr)
;; Convert c to formula cell (if necessary) with formula expr.

(destroy-cell! c)
;; Removes c from the cell graph so it can be GC'd. It's an error
;; to destroy a cell if other cells refer to it in their formulas.
```

### Cell Type Internals

The `Cell` type internals can be used to achieve more advanced goals.

```clojure
;; Get the previous value of a cell.
(.-prev my-cell)

;; Get a list of cells a cell depends on.
(.-sources my-cell)

;; Get a list of cells that depend on a cell.
(.-sinks my-cell)

;; Get a cell's "thunk", a function of no arguments that recomputes
;; and sets the cell's value.
(.-thunk my-cell)
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
[8]: https://github.com/lynaghk/todoFRP/tree/master/todo/hlisp-javelin
[9]: https://github.com/tailrecursion/javelin#formulas
