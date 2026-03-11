{- |
Copyright   :  (c) Henning Thielemann 2008-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

A lazy number type, which is a generalization of lazy Peano numbers.
Comparisons can be made lazy and
thus computations are possible which are impossible with strict number types,
e.g. you can compute @let y = min (1+y) 2 in y@.
You can even work with infinite values.
However, depending on the granularity,
the memory consumption is higher than that for strict number types.
This number type is of interest for the merge operation of event lists,
which allows for co-recursive merges.
-}
module Numeric.NonNegative.Chunky
   (T, fromChunks, fromNumber, toChunks, toNumber,
    zero, normalize, isNull, isPositive,
    divModStrict,
   ) where

import Numeric.NonNegative.ChunkyPrivate
