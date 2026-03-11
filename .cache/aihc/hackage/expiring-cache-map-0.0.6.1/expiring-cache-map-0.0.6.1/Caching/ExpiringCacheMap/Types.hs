{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Types
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Types common to "Caching.ExpiringCacheMap.OrdECM" and "Caching.ExpiringCacheMap.HashECM".
-- 

module Caching.ExpiringCacheMap.Types (
    -- * Configuration
    CacheSettings(..),
    -- * Cache encapsulation
    ECM,
    CacheState,
    -- * Types
    TimeUnits,
    ECMMapSize,
    ECMULength,
    ECMIncr,
    -- * Types for state function
    ECMNewState,
    ECMEnterState,
    ECMReadState,
) where

import Caching.ExpiringCacheMap.Utils.Types
import Caching.ExpiringCacheMap.Internal.Types

data CacheSettings =
  -- | A cache that maintains a key access history list to perform removals
  -- of /least recently used/ entries. Once the key-value map reaches 
  -- 'removalsize' keys, then a list of keys to keep in the map is determined 
  -- which is no larger than 'mapsize' size. Entries are removed only on 
  -- insertion of a new entry in the key-value map.
  -- 
  -- Key access history entries are prepended to the head of the LRU list, 
  -- if an existing entry for the key appears close to the head of the list
  -- it is moved to the head of the list, instead of growing the list. When the 
  -- LRU list reaches 'compactlistsize' items, it is compacted by removing 
  -- duplicate keys, by keeping only the most recent accumulator value for 
  -- that key.
  --
  CacheWithLRUList {
    mapsize :: ECMMapSize,
    removalsize :: ECMMapSize,
    compactlistsize :: ECMULength
  }
