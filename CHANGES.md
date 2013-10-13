# javelin

## 2.0.0-SNAPSHOT

*Sun Oct 13 18:24:15 EDT 2013*

* Remove `mx` macro.
* Add `macroexpand-all` macro.

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
