{-# LANGUAGE OverloadedStrings #-}

module TestHashECMWithTestSequenceInvalidating (
    testWithTestSequenceInvalidating
) where

import Caching.ExpiringCacheMap.HashECM (newECMForM, lookupECM, CacheSettings(..), consistentDuration, invalidate, invalidateCache, keysCached, keysNotExpired)
import qualified Caching.ExpiringCacheMap.Utils.TestSequence as TestSeq

import TestECMWithTestSequenceCommonInvalidating

import qualified Data.ByteString.Char8 as BS
import System.Exit (exitFailure)

testWithTestSequenceInvalidating = do
  (TestSeq.TestSequenceState (_, events',    _), return_value) <- TestSeq.runTestSequence test'
  (TestSeq.TestSequenceState (_, events'',   _), return_value) <- TestSeq.runTestSequence test''
  (TestSeq.TestSequenceState (_, events''',  _), return_value) <- TestSeq.runTestSequence test'''
  (TestSeq.TestSequenceState (_, events'''', _), return_value) <- TestSeq.runTestSequence test''''
  (TestSeq.TestSequenceState (_, events''''',  _), return_value) <- TestSeq.runTestSequence test'''''
  (TestSeq.TestSequenceState (_, events'''''', _), return_value) <- TestSeq.runTestSequence test''''''
  if pattern'I    (filt events') &&
     pattern''I   (filt events'') &&
     pattern'''I  (filt events''') &&
     pattern''''I (filt events'''') &&
     pattern'''''I  (filt events''''') &&
     pattern''''''I (filt events'''''')
    then do
      putStrLn "Passed TestHashECMWithTestSequenceInvalidating"
      -- printOutEvents events' events'' events''' events''''
      return ()
    else do
      printOutFailedPatternI "TestHashECMWithTestSequenceInvalidating.testWithTestSequenceInvalidating"
        (filt events') (filt events'') (filt events''') (filt events'''') (filt events''''') (filt events'''''')
      printOutEventsI events' events'' events''' events'''' events''''' events''''''
      exitFailure
  where
    filt = filter someEventsOnlyI . reverse
    commonreadnumber =
      (\state _id -> do number <- TestSeq.readNumber
                        return (state, number))
    
    newTestECM valreq timecheck =
      newECMForM valreq
        (TestSeq.getCurrentTime >>= return)
        timecheck
        (CacheWithLRUList 6 6 12)
        TestSeq.newTestSVar TestSeq.enterTestSVar TestSeq.readTestSVar
            
    test' = do
      filecache <- newTestECM
        (consistentDuration 100 commonreadnumber) -- Duration between access and expiry time
        12000 -- Time check frequency
      testLookupsI (lookupECM filecache) (invalidate filecache) (invalidateCache filecache) (keysCached filecache) (keysNotExpired filecache)
    
    test'' = do
      filecache <- newTestECM
        (consistentDuration 100 commonreadnumber) -- Duration between access and expiry time
        1 -- Time check frequency
      testLookupsI (lookupECM filecache) (invalidate filecache) (invalidateCache filecache) (keysCached filecache) (keysNotExpired filecache)
    
    test''' = do
      filecache <- newTestECM
        (consistentDuration 1 commonreadnumber) -- Duration between access and expiry time
        12000 -- Time check frequency
      testLookupsI (lookupECM filecache) (invalidate filecache) (invalidateCache filecache) (keysCached filecache) (keysNotExpired filecache)
    
    test'''' = do
      filecache <- newTestECM
        (consistentDuration 1 commonreadnumber) -- Duration between access and expiry time
        1 -- Time check frequency
      testLookupsI (lookupECM filecache) (invalidate filecache) (invalidateCache filecache) (keysCached filecache) (keysNotExpired filecache)
    
    test''''' = do
      filecache <- newTestECM
        (consistentDuration 50 commonreadnumber) -- Duration between access and expiry time
        12000 -- Time check frequency
      testLookupsI (lookupECM filecache) (invalidate filecache) (invalidateCache filecache) (keysCached filecache) (keysNotExpired filecache)
    
    test'''''' = do
      filecache <- newTestECM
        (consistentDuration 50 commonreadnumber) -- Duration between access and expiry time
        1 -- Time check frequency
      testLookupsI (lookupECM filecache) (invalidate filecache) (invalidateCache filecache) (keysCached filecache) (keysNotExpired filecache)

