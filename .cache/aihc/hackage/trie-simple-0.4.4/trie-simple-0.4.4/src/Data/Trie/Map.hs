module Data.Trie.Map(
  -- * Type
  TMap(),
  -- * Queries
  match,
  lookup,
  lookupPrefixes,
  member, notMember,
  null, count,
  keys, elems,
  -- * Construction
  empty, just,
  singleton,

  -- * Single item modification
  insertWith, insert,
  deleteWith, delete,

  adjust, revise, update, alter,

  -- * Combine
  --
  -- These functions behave in the same way as corresponding
  -- functions from "Data.Map".
  union, unionWith,
  intersection, intersectionWith,
  difference, differenceWith,
  appendWith,

  -- * Conversion
  toList, fromList, fromListWith,
  toAscList, fromAscList, fromAscListWith,
  toMap, fromMap,
  keysTSet, fromTSet,

  -- * Parsing
  toParser, toParser_, toParser__,

  -- * Traversing with keys
  traverseWithKey, mapWithKey, foldMapWithKey, foldrWithKey,
)
where

import           Prelude              hiding (lookup, null)

import           Data.Trie.Map.Hidden
