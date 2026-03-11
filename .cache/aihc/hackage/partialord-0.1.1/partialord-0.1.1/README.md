# partialord

A data structure supporting partial orders, and some helpful
associated routines.

## Other packages

An alternative package is already available, namely
  [partial-order](https://hackage.haskell.org/package/partial-order).
Differences include:
* PartialOrd has a comparison valued in Maybe Ordering; we use a fresh
  type with four constructors.
* Where types have several natural partial orderings, we provide
  newtypes rather than choosing one.
* We pay slightly more attention to algorithmic complexity.