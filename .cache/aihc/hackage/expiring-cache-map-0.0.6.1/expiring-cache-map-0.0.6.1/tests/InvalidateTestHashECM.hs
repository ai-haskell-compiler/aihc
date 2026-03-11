{-# LANGUAGE OverloadedStrings #-}

module InvalidateTestHashECM (
    tests
) where

import Caching.ExpiringCacheMap.HashECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)
import qualified Caching.ExpiringCacheMap.HashECM as ECM (invalidate, invalidateCache, keysCached, keysNotExpired)

import InvalidateTestCommon (test1Common, test2Common)

import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock
import Control.Concurrent (threadDelay)


tests = do
  test1
  test2
  return ()

test1 = do
  filecache1 <- newECMIO
        (consistentDuration 100
          (\state id -> do BS.putStrLn "Reading a file again..."
                           return (state, ("file contents" :: BS.ByteString) )))
        (return 1)
        12000 -- Value to modulo with cache state accumulator to determine time check frequency.
        (CacheWithLRUList
          100     -- Expected size of key-value map when removing elements.
          100     -- Size at when to remove items from key-value map.
          200 )
  test1Common (lookupECM filecache1) (ECM.invalidate filecache1) (ECM.keysCached filecache1) (ECM.invalidateCache filecache1)
  return ()

test2 = do
  filecache2 <- newECMIO
        (consistentDuration 0
          (\state id -> do BS.putStrLn "Reading a file again..."
                           return (state, ("file contents" :: BS.ByteString) )))
        (do time <- POSIX.getPOSIXTime
            return (round (time * 100)))
        1 -- Value to modulo with cache state accumulator to determine time check frequency.
        (CacheWithLRUList
          6     -- Expected size of key-value map when removing elements.
          6     -- Size at when to remove items from key-value map.
          12 )
  test2Common (lookupECM filecache2) (ECM.keysCached filecache2) (ECM.keysNotExpired filecache2)
  return ()
