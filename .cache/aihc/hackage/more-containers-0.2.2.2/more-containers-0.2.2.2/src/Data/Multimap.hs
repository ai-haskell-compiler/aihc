{-# LANGUAGE CPP #-}

-- | This modules provides a strict multimap implementation. To avoid collision with Prelude
-- functions, it is recommended to import this module qualified:
--
-- > import qualified Data.Multiset as Mmap
--
-- All complexities below use /m/ for the number of distinct keys and /n/ for the total number of
-- entries in the multimap.
module Data.Multimap (
  -- * Types
  -- ** Generic
  Multimap, Group,
  -- ** Specific
  ListMultimap,
  SeqMultimap,
  SetMultimap,
  -- * Construction
  empty, singleton,
#if __GLASGOW_HASKELL__ >= 708
  fromList,
#endif
  fromListWith, fromGroupList, fromMap,
  -- * Tests and accessors
  null,
  size, distinctSize,
  member, notMember,
  find, (!),
  -- * Modification
  prepend, prependMany,
  append, appendMany,
  deleteMany,
  modifyMany, modifyManyF,
  -- * Maps and filters
  mapGroups,
  filter, filterGroups,
  -- * Conversion
  toList, toGroupList, toMap,
  -- * Other
  keys, keysSet, keysMultiset,
#if __GLASGOW_HASKELL__ >= 708
  inverse,
#endif
  inverseWith,
  maxViewWith, minViewWith
) where

import Prelude ()

import Data.Multimap.Generic
import Data.Multimap.List (ListMultimap)
import Data.Multimap.Seq (SeqMultimap)
import Data.Multimap.Set (SetMultimap)
