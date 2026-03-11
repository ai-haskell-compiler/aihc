0.4.2.2 - 2024-01-01
--------------------

* Support tasty 1.5

0.4.2.1 - 2023-05-02
--------------------

* Remove flaky `pass` test
* Turn example tests into executables

0.4.2.0 - 2023-04-27
--------------------

* Remove Bootstrap
* Remove JQuery
* Style overhaul
* Visual tree heirarchy indication
* Ellide tree paths that don't lead to failed tests on load
* 'Expand all' button
* Highlight descendents of groups on hover
* Output less DOM nodes
* Automatic dark/light theme via the `prefers-color-scheme` media query

0.4.1.4 - 2021-01-13
--------------------

Documentation improvements

0.4.1.3 - 2021-01-13
--------------------

Update to tasty-1.4

0.4.1.2 - 2018-06-27
--------------------

Fix compatibility with GHC 8.4


0.4.1.1 - 2015-11-10
--------------------

- Report running time for tests.

0.4.1 - 2014-11-18
------------------

- Export `AssetsPath`.

0.4 - 2014-11-11
----------------

- Add option to load bootstrap assets externally.

0.3
---

This release is mainly about upgrading to bootstrap3.

* Modifications to bootstrap are done directly in its source, which is
  now maintained as a fork at https://github.com/jdnavarro/bootstrap. The
  modifications are mainly about inlining resources.
* Icons are now true fonts instead of image sprites.
* Test trees are represented as [*media
  objects*](http://getbootstrap.com/components/#media).
* Remove dependendency to bootstrap-tree.
* Upgrade to `tasty-0.10`.
* Fix HTML malformation.

0.2
---
* Remove forgotten `undefined` that made failing tests crash.
