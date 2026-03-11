{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Utils.Types
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Simple types.
-- 

module Caching.ExpiringCacheMap.Utils.Types (
    -- * Types
    TimeUnits,
    ECMMapSize,
    ECMULength,
    ECMIncr,
) where

import Data.Word (Word32)

-- | Integer involved in the time units used to determine when an item expires.
-- The time units used can be any arbitrary integer time representation, such
-- as seconds or milliseconds for examples. They can also be deterministic time
-- steps in a sequencing monad.
--
type TimeUnits = Int

-- | Integer involved in the size of a key-value map.
type ECMMapSize = Int

-- | Integer involved in the length of the usage history list.
type ECMULength = Int

-- | Unsigned integer ('Word32') involved in the cache state incrementing accumulator.
type ECMIncr = Word32
