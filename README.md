# Javelin

<img src="https://raw.github.com/alandipert/javelin/master/img/javelin.png?login=micha&token=b172f1b97acb55c16867dc106e30c646"
alt="tailrecursion/javelin logo" title="tailrecursion/javelin logo"
align="right" width="152"/>

Spreadsheet-like Functional Reactive Programming (FRP) in
ClojureScript.  This library is usable but under construction and
subject to frequent change.

### Example

```clojure
(ns your-ns
  (:require tailrecursion.javelin) ;; necessary if compiling in advanced mode
  (:require-macros [tailrecursion.javelin.macros :refer [cell]]))

(defn start []
  (let [a (cell 0)            ;; value cell of 0.
        b (cell (inc a))      ;; formula cell of a+1.
        c (cell (+ 123 a b))] ;; formula cell of a+b+123.
    (cell (.log js/console c))
    ;; c's initial value, 124, is printed.
    (swap! a inc)
    ;; a was incremented, and its new value propagated (consistently)
    ;; through b and c.  c's new value, 126, is printed to the console.
    ))
```

### Dependency [![Build Status](https://travis-ci.org/tailrecursion/javelin.png?branch=master)](https://travis-ci.org/tailrecursion/javelin)

```clojure
[tailrecursion/javelin "1.0.0-SNAPSHOT"]
```

### Demos

For more usage examples, see the [Javelin
Demos](https://github.com/tailrecursion/javelin-demos) repository.

You may see some version of the demos running at
[http://tailrecursion.com/~alan/javelin-demos/](http://tailrecursion.com/~alan/javelin-demos/).

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

Both kinds of cell are created with the Javelin `cell` macro. It
expects a single argument. If the argument is a number, string,
keyword, anonymous function, or quoted expression the cell will be an
input cell. Otherwise, the new cell will be a formula cell.

```clojure
(def a (cell 42))       ;; input cell containing the value 42
(def b (cell '(+ 1 2))) ;; input cell containing the value 3
(def c (cell '{:x 10})) ;; input cell containing the value {:x 10}

(def d (cell {:x y}))   ;; formula cell containing the value (hash-map :x y), updated when y changes
(def e (cell (+ a 1)))  ;; formula cell containing the value a+1, updated when a changes

(reset! a 7)   ;; update the value contained by the input cell explicitly
(swap! e inc)  ;; no! e is a formula cell; it updates itself!
```

### Formulas

Formula cells may refer to themselves with the `~value` idiom, where
`value` is the initial value to use (the cell has no "self" value
before it has evaluated itself the first time). Self references refer
to the value in the cell prior to updating, of course.

```clojure
(let [a (cell 0)             ;; input cell
      b (cell (conj ~[] a))] ;; formula cell w/ self reference, ~[]
  (cell (.log js/console (pr-str b)))
  ;; [0] is printed first
  (swap! a inc)
  (swap! a inc)
  ;; then [0 1] and [0 1 2] are printed
  )
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
