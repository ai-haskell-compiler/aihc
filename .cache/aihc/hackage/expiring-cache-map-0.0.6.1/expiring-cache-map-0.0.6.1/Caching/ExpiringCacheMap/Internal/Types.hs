{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Internal.Types
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Types used by internal functions and as the opaque types exported by other 
-- modules, assume these type definitions to change from version to version.
-- 

module Caching.ExpiringCacheMap.Internal.Types (
    -- * Cache internals
    ECM(..),
    CacheState(..),
    ECMNewState,
    ECMEnterState,
    ECMReadState
) where

import qualified Control.Concurrent.MVar as MV
import Caching.ExpiringCacheMap.Utils.Types

type ECMNewState a b s m k v = (CacheState s m k v) -> a (b (CacheState s m k v))

type ECMEnterState a b s m k v = b (CacheState s m k v) -> ((CacheState s m k v) -> a ((CacheState s m k v), v)) -> a v

type ECMReadState a b s m k v = b (CacheState s m k v) -> a (CacheState s m k v)

-- | The cache state.
newtype CacheState s m k v =
  CacheState (Maybe s, m k (TimeUnits, TimeUnits, v), ECMMapSize, ([(k, ECMIncr)], ECMULength), ECMIncr)

-- | The type that encapsulates a cache map.
newtype ECM a b s m k v = ECM ( b (CacheState s m k v),
                  Maybe s -> k -> a (TimeUnits, (Maybe s, v)),
                  a TimeUnits,
                  ECMMapSize,
                  -- TimeUnits,
                  ECMIncr,
                  ECMULength,
                  ECMULength,
                  ECMEnterState a b s m k v,
                  ECMReadState a b s m k v)
