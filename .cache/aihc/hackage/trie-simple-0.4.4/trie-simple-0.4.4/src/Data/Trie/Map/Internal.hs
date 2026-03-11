{- |
This module exposes internal representation of @TMap@.
TMap has one invariant condition:

* Subtrees of an @TMap@ should not be empty.

For example, consider following tree structure which is valid:

  > > fromList [("a",1), ("aa", 2), ("bc", 3)]
  > Root
  >   'a' -> 1
  >     'a' -> 2
  >   'b' ->
  >     'c' -> 3

Adding redundant node which represents empty map does not change
what an @TMap@ represents.

  > Root
  >   'a' -> 1
  >     'a' -> 2
  >   'b' ->
  >     'c' -> 3
  >   'd' ->
  >     'e' ->
  >       'f' ->

But such @TMap@ should not exist because it confuses @Eq@ and @Ord@
instances and @null@ function.
-}
module Data.Trie.Map.Internal(
  -- * Types
  TMap(..),
  Node(..),
  foldTMap,
)
where

import Data.Trie.Map.Hidden
