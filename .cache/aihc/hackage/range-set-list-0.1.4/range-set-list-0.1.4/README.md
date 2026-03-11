# range-set-list

[![Build Status](https://travis-ci.org/phadej/range-set-list.svg?branch=travis-expr)](https://travis-ci.org/phadej/range-set-list)
[![Hackage](https://img.shields.io/hackage/v/range-set-list.svg)](http://hackage.haskell.org/package/range-set-list)
[![Stackage LTS 2](http://stackage.org/package/range-set-list/badge/lts-2)](http://stackage.org/lts-2/package/range-set-list)
[![Stackage LTS 3](http://stackage.org/package/range-set-list/badge/lts-3)](http://stackage.org/lts-3/package/range-set-list)
[![Stackage Nightly](http://stackage.org/package/range-set-list/badge/nightly)](http://stackage.org/nightly/package/range-set-list)

A few trivial implementations of range sets.

You can find the package (and its documentation) on [hackage](http://hackage.haskell.org/package/range-set-list).

This module is intended to be imported qualified, to avoid name
clashes with Prelude functions, e.g.,

```haskell
import Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet
```

This package contains two implementations of exactly the same interface, plus one specialization, all of which provide exactly the same behavior:

* "Data.RangeSet.List" implements the simplest `RSet` based on _list_. Set construction and manipulation is most efficient for this version, but lookups may require a full list traversal.
* "Data.RangeSet.Map" implements a slightly less simple `RSet` based on _map_. Construction and manipulation have more overhead in this version, but lookups are significantly faster, especially for large sets.
* "Data.RangeSet.IntMap" is simply a specialization of "Data.RangeSet.Map" to Ints based on IntMap.

Compared to [`Data.Set`](http://hackage.haskell.org/package/containers-0.5.4.0/docs/Data-Set.html),
this module also imposes an [`Enum`](http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#t:Enum)
constraint for many functions.
We must be able to identify consecutive elements to be able to _glue_ and _split_ ranges properly.

The implementation assumes that

```haskell
x < succ x
pred x < x
```

and there aren't elements in between (not true for `Float` and `Double`).
Also `succ` and `pred` are never called for largest or smallest value respectively.
