
# Revision history for circular


## Unreleased changes


## 0.4.0.3

-   Tooling and compilation with GHC 9.2.3.


## 0.4.0.2

-   Tooling updates related to GHC 9.2.1.


## 0.4.0.1

-   Minor changes mostly related to tooling.
-   Nix support.


## 0.4.0.0

-   Do not export data constructors nor record fields.
-   `fromVectorWith`.


## 0.3.1.1

Cabal changes, versioning changes.


## 0.3.1

-   Change monadic folds so that commutativity is not anymore required.
-   Provide `foldKM`, a fold over the last k elements on the stack.


## 0.3.0

-   Bugfix `take`.
-   Make functions more consistent (`fromVector` and `toVector` now both work on
    mutable stacks).
-   Sort functions.
-   Improve documentation.


## 0.2.0

-   Complete rewrite using mutable vectors. A monadic interface is required now,
    but it is much cleaner in every other sense.


## 0.1.1

-   Remove `mean`.
-   Add benchmark.
-   Many small improvements.

