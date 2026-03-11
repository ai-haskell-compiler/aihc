{-# LANGUAGE OverloadedStrings #-}

module InvalidateCacheTestOrdECM (
    tests
) where

import Caching.ExpiringCacheMap.HashECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)
import qualified Caching.ExpiringCacheMap.HashECM as ECM (invalidateCache, keysCached, keysNotExpired)
import qualified Data.ByteString.Char8 as BS

import InvalidateCacheTestCommon (test1Common)

tests = test1

test1 = do
  filecache3 <- newECMIO
        (consistentDuration 100
          (\state id -> do BS.putStrLn "Reading a file again..."
                           return (state, ("file contents" :: BS.ByteString) )))
        (return 1)
        12000 -- Value to modulo with cache state accumulator to determine time check frequency.
        (CacheWithLRUList
          100   -- Expected size of key-value map when removing elements.
          100   -- Size at when to remove items from key-value map.
          200 )
  test1Common (lookupECM filecache3) (ECM.keysCached filecache3) (ECM.invalidateCache filecache3)
  return ()
