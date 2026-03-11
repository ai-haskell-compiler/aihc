{-|
This module provides a type @TSet c@, which is a set of list of some
characters. It serves almost same purpose to @Set [c]@, and functions of
this module mirrors functions with same name from "Data.Set" module.

The advantages to use this module over "Data.Set" are:

* Faster 'member'.
* Partial match provided by 'beginWith' function.
* Efficient 'append', 'prefixes', and 'suffixes' functions.

But notice for some disadvantages:

* Some operations are slower than @Set [c]@. Especially, 'count' is much
  much slower than 'Set.size' (because @Set.size@ is already recorded in the
  data structure). Consider @TSet.count@ be like @length@ of list.
* Constructed @TSet c@ from a list of lists @[[c]]@ do not share each member
  lists with original list unlike @Set [c]@ does. This means holding both
  @TSet c@ and @[[c]]@ in memory consumes much more memory than @Set [c]@ and
  @[[c]]@.

-}
module Data.Trie.Set(
  -- * Types
  TSet(),
  -- * Queries
  member, notMember,
  beginWith,
  null, count, enumerate,
  foldMap, foldr, foldl', 
  -- * Construction
  empty, epsilon,
  singleton,
  insert, delete,
  -- * Combine
  union, intersection, difference,
  append,
  -- * Other operations
  prefixes, suffixes, infixes,
  -- * Conversion
  fromList, toList,
  fromAscList, toAscList,
  fromSet, toSet,
  -- * Parsing
  toParser, toParser_
)
where

import Prelude hiding (Foldable(..))
import Data.Trie.Set.Hidden
