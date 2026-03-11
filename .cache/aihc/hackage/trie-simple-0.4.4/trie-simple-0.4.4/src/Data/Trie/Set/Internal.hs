{- |
This module exposes internal representation of @TSet@.
TSet has one invariant condition:

* Subtrees of an @TSet@ should not be empty.

For example, consider following tree structure which is valid:

  > > fromList ["a", "aa", "bc"]
  > Root -> False
  >   'a' -> True
  >     'a' -> True
  >   'b' -> False
  >     'c' -> True

Adding redundant node which represents empty set does not change
what an @TSet@ represents.

  > Root -> False
  >   'a' -> True
  >     'a' -> True
  >   'b' -> False
  >     'c' -> True
  >   'd' -> False
  >     'e' -> False
  >       'f' -> False

But such @TSet@ should not exist because it confuses @Eq@ and @Ord@ instances and @null@ function.
-}
module Data.Trie.Set.Internal(
  TSet(..),
  Node(..),
  foldTSet, paraTSet
)
where

import Data.Trie.Set.Hidden
