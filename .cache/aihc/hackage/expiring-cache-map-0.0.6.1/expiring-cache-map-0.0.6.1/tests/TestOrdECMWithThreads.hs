{-# LANGUAGE OverloadedStrings #-}

--
-- Test OrdECM with threads
--

module TestOrdECMWithThreads (
    testWithThreads
) where

import Control.Concurrent (forkIO, threadDelay, yield)
import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as M

import Caching.ExpiringCacheMap.OrdECM
import Caching.ExpiringCacheMap.Internal.Internal (getStatsString)

import System.Timeout (timeout)
import System.Exit (exitFailure)

testWithThreads = do
  res <- timeout 60000000 testWithThreads'
  case res of
    Nothing -> exitFailure
    Just () -> return ()

testWithThreads' = do
  ecm <- newECMIO
            (consistentDuration 10
              (\state id -> do LBS.putStrLn id; return (state, [])))
            (do time <- POSIX.getPOSIXTime
                return (round (time * 100)))
            120
            (CacheWithLRUList 6 6 12 ) :: IO (ECM IO MV.MVar () M.Map LBS.ByteString [Int])
  t1 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.2"
      yield -- threadDelay 2
      return ())
      [0..500]
    MV.putMVar t1 True
  t2 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.3"
      yield -- threadDelay 3
      return ())
      [0..333]
    MV.putMVar t2 True
  t3 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.5"
      yield -- threadDelay 5
      return ())
      [0..200]
    MV.putMVar t3 True
  t4 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.7"
      yield -- threadDelay 7
      return ())
      [0..142]
    MV.putMVar t4 True
  t5 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.11"
      yield -- threadDelay 11
      return ())
      [0..90]
    MV.putMVar t5 True
  t6 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.13"
      yield -- threadDelay 13
      return ())
      [0..76]
    MV.putMVar t6 True
  t7 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.17"
      yield -- threadDelay 17
      return ())
      [0..58]
    MV.putMVar t7 True
  t8 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.19"
      yield -- threadDelay 19
      return ())
      [0..52]
    MV.putMVar t8 True
  t9 <- MV.newEmptyMVar
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.23"
      yield -- threadDelay 23
      return ())
      [0..43]
    MV.putMVar t9 True
  untilDone [t1,t2,t3,t4,t5,t6,t7,t8,t9]
  c <- getStatsString ecm
  putStrLn c
  return ()
  where
    untilDone [] = return ()
    untilDone (t:tr) = MV.takeMVar t >> untilDone tr
