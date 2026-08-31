-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}

-- This is a non-exposed internal module
--
-- The interface in this module comes from containers-0.5.5.1:Data.Map.Base [1].
-- This local implementation avoids a dependency on the containers package.
--
-- [1] see https://hackage.haskell.org/package/containers-0.5.5.1
--
-- The original code is BSD-licensed and copyrighted by Daan Leijen, Andriy Palamarchuk, et al.

module GHC.Boot.TH.Lib.Map
  ( Map,
    empty,
    insert,
    GHC.Boot.TH.Lib.Map.lookup,
  )
where

import Prelude hiding (lookup)

newtype Map k a = Map [(k, a)]

empty :: Map k a
empty = Map []
{-# INLINE empty #-}

lookup :: (Eq k) => k -> Map k a -> Maybe a
lookup !key (Map entries) = lookupEntries key entries
{-# INLINEABLE lookup #-}

lookupEntries :: (Eq k) => k -> [(k, a)] -> Maybe a
lookupEntries _ [] = Nothing
lookupEntries key ((entryKey, value) : entries) =
  if key == entryKey then Just value else lookupEntries key entries

insert :: (Eq k) => k -> a -> Map k a -> Map k a
insert !key value (Map entries) = Map (insertEntry key value entries)
{-# INLINEABLE insert #-}

insertEntry :: (Eq k) => k -> a -> [(k, a)] -> [(k, a)]
insertEntry key value [] = [(key, value)]
insertEntry key value ((entryKey, entryValue) : entries) =
  if key == entryKey
    then (key, value) : entries
    else (entryKey, entryValue) : insertEntry key value entries
