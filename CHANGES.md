# javelin

## 3.9.2

* Remove unused dependency tailrecursion/cljs-priority-map

## 3.9.1

* Move from boot to tools-deps
* Setup ci using github actions
* Add clj-kondo config
* Improve performance of `javelin.core/cell?` (thanks @borkdude)

## 3.9.0

* Add formula-of, formulet macros
* Formula cells that cannot change will be tagged as constant
* Better support for lenses:
  - Added arity to formula function to accept lens update function.
  - Added arity to set-formula! function to accept lens update function.
  - Added arity to set-cell!= macro to accept lens update function.

* Various performance improvements:
  - Cell type fields are now native JS arrays, not CLJS collections.
  - Replace CLJS priority map used in propagation  with a sorted JS array.
  - Eliminate seqs wherever possible by looping instead.
  - Remove redundant operations during cell creation.
  - Add simpler, more efficient impl. for simple cell-doseq use case.

* Misc:
  - Add docstrings for everything.
  - Use clojure.core/destructure instead of cljs.core/destructure when
    compiling in an older version of CLJS that does not have it.

## 3.8.5

* cell-let binds map destructuring
* Add formula to API overview in README

## 3.8.4

* Correct handling of catch, finally

## 3.8.3

* Add missing special forms, fix tests

## 3.8.2

* Update README with namepace changes
* Change deploy group from javelin to hoplon

## 3.8.1

* introduce formula function earlier on README
* Rename namespaces (BREAKING)

## 3.8.0

* Add meta to cells
* Move from lein to boot

## 3.7.2

* Improve javelin-clj, add tests

## 3.7.1

* Remove debug print, fix clojars deploy

## 3.7.0

* Accept "for" style bindings in cell-doseq 

## 3.6.3

* Fix issue with ::none appearing in formulas in dosync 

## 3.6.2

* Improve lenses examples and tests

## 3.6.1

* Update clojurescript version

## 3.6.0

*Fri Aug  8 13:15:23 EDT 2014*

* Add optional additional argument to `cell=` and `defc=` macros. When called
  with this additional argument they return lens cells.

## 3.5.0

*Thu Aug  7 18:45:31 EDT 2014*

* Add `formula?`, `lens?`, and `lens`.
* Fix issues with watches not being notified when formulas are mutated.
* Fix issues with watch notifications and transactions.
* Better performance.

## 3.4.0

*Wed Aug  6 11:13:56 EDT 2014*

* Add `dosync` macro.

## 3.3.2

*Tue Jul  1 00:27:56 EDT 2014*

* `lift` deprecated and renamed to `formula`

## 3.3.1

* Bugfix: Fix issue with cell-let macro

## 3.3.0

* Bugfix: Fix cell-doseq, making it reactive.

## 3.2.0

* Add prop-cell

## 3.1.1

* cljs 0.0-2173 compat: IReset/ISwap support in Cell

## 3.1.0

* add cell-doseq test; dox
* Remove watches on destroy; add cell-let; cleanup

## 3.0.0

* Seqify collection cell arg to cell-map; remove mirroring macros

## 2.4.0

*Sun Nov 24 12:34:02 EST 2013*

* add clojure implementation

## 2.3.0

*Wed Oct 30 16:09:26 EDT 2013*

* add `cell-map` function and `cell-doseq` macro

## 2.2.1

*Mon Oct 21 23:33:21 EDT 2013*

* add `make-require` and `make-require-macros`

## 2.2.0

*Mon Oct 21 16:41:17 EDT 2013*

* add `refer-all` macro

## 2.1.1

*Sun Oct 20 16:25:35 EDT 2013*

* rollback unquote fix in 2.0.3

## 2.1.0

*Sun Oct 20 14:22:48 EDT 2013*

* Feature: add `alts!` macro

## 2.0.3

*Sat Oct 19 18:24:43 EDT 2013*

* Bugfix: issue with syntax-unquoted references to cells in formulas

## 2.0.2

*Thu Oct 17 15:12:29 EDT 2013*

* Update dependencies to versions compatible with new CLJS version.

## 2.0.1

*Thu Oct 17 02:31:41 EDT 2013*

* Bugfix: issue with arg destructuring in anon fns in formulas

## 2.0.0

*Wed Oct 16 12:23:32 EDT 2013*

## 2.0.0-SNAPSHOT

*Wed Oct 16 11:12:01 EDT 2013*

* Use hoisting instead of lifting when constructing formulas.
* Unsupported special forms are now down to just `ns`, `def`, `deftype*`,
  and `defrecord*`.
* It is no longer an error to destroy a cell that other cells reference
  in their formulas.

*Mon Oct 14 14:08:09 EDT 2013*

* Add `defc` and `defc=` macros.

*Sun Oct 13 18:24:15 EDT 2013*

* Remove `mx` macro.
* Add `macroexpand-all` macro.
* Unsupported special forms in formulas now cause compile-time exception.
* Fix issue where local bindings were interpreted as special forms with the
  same name.
* The `cell?` function returns the cell or nil instead of boolean.
* Add `input?` macro.
* Watches can be added to input and formula cells. IWatchable is now fully
  supported again by all Cells.
* Move `tailrecursion.javelin.macros` namespace to `tailrecursion.javelin`.
* Remove `cell?`, `input?`, `cell`, `set-cell!`, and `destroy-cell!`
  macros&mdash;use the functions with corresponding names instead.

*Tue Oct  8 19:28:37 EDT 2013*

* Add `cell?`, `set-cell!`, `set-cell!=`, and `destroy-cell!` macros.

*Sun Oct  6 16:32:01 EDT 2013*

* Throw exception when `swap!` or `reset!` is applied to a formula cell.
* Watches can no longer be added to formula cells. Use cells instead.
* Remove macros `cfn` and `defcfn` from public API.
* Remove convenience functions `route*`, `timer*`, `log`, `timeout`, and
  `interval` from public API.
* Remove `self?` from public API and self-referential code.
* The `cell` macro creates only input cells; `cell=` creates only formula
  cells.
* Changes in the way macro builds formulas:
  * Quoted expressions are not walked but remain quoted.
  * The `unquote` form is repurposed: argument is evaluated in place and not
    walked.
  * The `unquote-splicing` form is interpreted as the composition of `unquote`
    and `deref`.

*Mon Sep 30 15:22:11 EDT 2013*

* Remove `done` property from `Cell` type.
* Remove `done!` from public API.
