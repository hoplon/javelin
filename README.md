<img src="https://raw.github.com/hoplon/javelin/master/img/javelin.png"
alt="tailrecursion/javelin logo" title="tailrecursion/javelin logo"
align="right" width="152"/>

# Javelin [![Build Status][1]][2]

Spreadsheet-like dataflow programming in ClojureScript.

[](dependency)
```clojure
[hoplon/javelin "3.9.0"] ;; latest release
```
[](/dependency)

### Example

```clojure
(ns your-ns
  (:require [javelin.core :refer [cell] :refer-macros [cell=]]))

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

There are many more examples in [the Javelin tests][12].

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

* are created with the `formula` function or the `cell=` and `defc=` macros.
* are updated _reactively_ according to a formula.
* are read-only&mdash;updating a formula cell via `swap!` or `reset!`
  is an error (unless it's a [lens](#lenses)).

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

## Transactions

The `dosync` macro facilitates atomic, transactional updates to cells. This can
be used to coordinate or batch updates such that propagation occurs only once
for the entire transaction instead of once for each individual cell update.

For example, consider the following program:

```clojure
(defc a 100)
(defc b 200)

(cell= (print "a + b =" (+ a b)))
;=> LOG: a + b = 300

(do
  (swap! a inc)
  (swap! a inc)
  (swap! b inc))
;=> LOG: a + b = 301
;=> LOG: a + b = 302
;=> LOG: a + b = 303
```

Notice how calling `swap!` on cells `a` and `b` individually causes the
anonymous cell to print the sum three times–once for each update to `a`
and `b`.

Using `dosync` these updates can be made inside of a transaction during which
propagation is suspended:

```clojure
(defc a 100)
(defc b 200)

(cell= (print "a + b =" (+ a b)))
;=> LOG: a + b = 300

(dosync
  (swap! a inc)
  (swap! a inc)
  (swap! b inc))
;=> LOG: a + b = 303
```

The sum is only logged a single time, even though `a` and `b` were updated
multiple times.

> **Note:** During a transaction the effects of `swap!` and `reset!` are
> immediately visible for input cells, but not for formula cells. Formula
> cells are updated and watchers are notified only after the transaction is
> complete.

## Lenses

A [lens][14] is a tool for extracting and updating a part of a data
structure. A lens then is defined by a complementary pair of getter and setter 
functions. Moreover lenses are composable, in that multiple lenses can be
combined to access and alter the contents of structures of structures.

Formula cells can easily be defined with a getter method for accessing some
selected content of a data structure. Formula cells as we have defined them
are read-only. but simply by providing a setter function as a second argument
they become bi-directional lenses.

Lenses still serve as formula cells, allowing you to access the content of
a data structure. But you can do a swap! or reset! on a lens to update
that same structure.

For example:

```clojure
(defn path-cell [c path]
  (cell= (get-in c path) (partial swap! c assoc-in path)))

(defc a {:a [1 2 3], :b [4 5 6]})
(def  c (path-cell a [:a]))

@c                       ;=> [1 2 3]
(swap! c pop)            ;=> [1 2]
@c                       ;=> [1 2]
@a                       ;=> {:a [1 2], :b [4 5 6]}

@c                       ;=> [1 2]
(reset! c :x)            ;=> :x
@c                       ;=> :x
@a                       ;=> {:a :x, :b [4 5 6]}

@c                       ;=> :x
(swap! a assoc :a [1 2]) ;=> {:a [1 2], :b [4 5 6]}
@c                       ;=> [1 2]
```

The `path-cell` function returns a converging lens whose formula focuses in
on a part of the underlying collection using `get-in`. The provided callback
takes the desired new value and updates the underlying collection accordingly
using `assoc-in`. The update propagates to the lens formula, thereby updating
the value of the lens cell itself.

Interestingly, transactions can be used to create diverging lenses, inverting
the above relationship between lens and underlying collection. Instead of
focusing the lens on a single collection to extract a part it, the lens can be
directed toward a number of individual cells to combine them into a single
collection.

For example:

```clojure
(defc  a 100)
(defc  b 200)
(defc= c {:a a, :b b} #(dosync (reset! a (:a %)) (reset! b (:b %))))

@a                       ;=> 100
@c                       ;=> {:a 100, :b 200}
(swap! c assoc :a 200)   ;=> {:a 200, :b 200}
@a                       ;=> 200
```

The `c` lens encapsulates the machinery of atomically updating both `a` and
`b` in the standard cell interface.

Converging and diverging lenses can be useful for low-impact, surgical
refactoring. They encapsulate the value and mutation semantic, eliminating
the need to modify existing code that references the underlying cells.

## Javelin API

Requiring the namespace and macros:

```clojure
(ns my-ns
  (:require
    [javelin.core
     :refer [cell? input? cell formula lens set-cell! alts! destroy-cell! cell-map]
     :refer-macros [cell= defc defc= set-cell!= dosync cell-doseq]]))
```

API functions and macros:

```clojure
(cell? c)
;; Returns c if c is a Cell, nil otherwise.

(input? c)
;; Returns c if c is an input cell, nil otherwise.

(formula? c)
;; Returns c if c is a formula cell, nil otherwise.

(lens? c)
;; Returns c if c is a lens cell, nil otherwise.

(cell expr)
;; Create new input cell with initial value expr.

((formula f) x y z)
;; Create a new formula cell with the formula function.
;; Analogous to (cell= (f x y z)) but is a function, not a macro.

(cell= expr)
;; Create new fomula cell with formula expr.

(cell= expr f)
;; Create new lens cell with formula expr and callback f. When swap! or reset!
;; is applied to the cell the callback is fired with the requested new value
;; provided as an argument. The callback does not manipulate the lens cell's
;; value directly, but it can swap! or reset! input cells (or do anything else),
;; possibly resulting in a new value being computed by the lens formula.

(defc symbol doc-string? expr)
;; Creates a new input cell and binds it to a var with the name symbol and
;; the docstring doc-string if provided.

(defc= symbol doc-string? expr)
;; Creates a new formula cell with formula expr and binds it to a var with the
;; name symbol and the docstring doc-string if provided.

(defc= symbol doc-string? expr f)
;; Creates a new lens cell with formula expr and callback f, and binds it to a
;; var with the name symbol and the docstring doc-string if provided.

(set-cell! c expr)
;; Convert c to input cell (if necessary) with value expr.

(set-cell!= c expr)
;; Convert c to formula cell (if necessary) with formula expr.

(set-cell!= c expr f)
;; Convert c to lens cell (if necessary) with formula expr and callback f.

(destroy-cell! c)
;; Disconnects c from the propagation graph so it can be GC'd.

(dosync exprs*)
;; Evaluates exprs (in an implicit do) in a transaction that encompasses exprs
;; and any nested calls. Cell propagation occurs only after all exprs have been
;; run, and propagation occurs only once. Only the final values of cells updated
;; within the transaction are propagated.

(alts! cs*)
;; Creates a formula cell whose value is a list of changed values in the cells cs.

(cell-map f c)
;; Given a cell c containing a seqable value of size n and a function f, returns
;; a sequence of n formula cells such that the ith cell's formula is (f (nth c i)).

(cell-let [binding-form c] body*)
;; Given a cell c and a binding form, binds names in the binding form to formula
;; cells containing the destructured values (these values will update as the
;; value of c changes) and evaluates the body expressions.

(cell-doseq seq-exprs body*)
;; Repeatedly executes the body expressions for side effects as doseq does, with
;; bindings as provided by "for", except that the binding forms are bound to
;; javelin cells, not values. The :while and :when filters are not supported at
;; this time.

(prop-cell prop-expr)
;; Returns a formula cell whose value is synced to the prop-expr, which is a
;; JavaScript property access expression, like (.-foo js/bar) for example.

(prop-cell prop-expr setter-cell callback?)
;; Given a property access expression (see above) prop-expr, a formula cell
;; setter-cell, and optionally a callback function, the JavaScript object
;; property specified by prop-expr is kept synced to the value in setter-cell
;; at all times. If the callback was provided it will be called whenever an
;; attempt is made to change the value of the property by means other than via
;; the setter-cell.
```

## Building and Running Tests

To build Javelin with advanced optimizations and run tests in PhantomJS:

```
boot test-javelin --advanced
```

For development you may want to forgo optimizations and test continuously:

```
boot watch speak test-javelin
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

[1]: https://travis-ci.org/hoplon/javelin.png?branch=master
[2]: https://travis-ci.org/hoplon/javelin
[3]: http://clojars.org/javelin/javelin
[4]: https://github.com/tailrecursion/javelin-demos
[5]: https://dl.dropboxusercontent.com/u/12379861/javelin_demos/index.html
[7]: https://github.com/lynaghk/todoFRP/tree/master/todo/javelin
[8]: https://github.com/hoplon/demos/tree/master/todoFRP
[9]: https://github.com/hoplon/javelin#formulas
[12]: https://github.com/hoplon/javelin/blob/master/test/javelin/core_test.cljs
[14]: https://www.thoughtworks.com/talks/journey-through-the-looking-glass-and-what-i-found-there
