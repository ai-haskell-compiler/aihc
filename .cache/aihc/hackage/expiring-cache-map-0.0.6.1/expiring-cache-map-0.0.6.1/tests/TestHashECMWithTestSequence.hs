{-# LANGUAGE OverloadedStrings #-}

module TestHashECMWithTestSequence (
    testWithTestSequence
) where

import Caching.ExpiringCacheMap.HashECM (newECMForM, lookupECM, CacheSettings(..), consistentDuration)
import qualified Caching.ExpiringCacheMap.Utils.TestSequence as TestSeq

import TestECMWithTestSequenceCommon

import qualified Data.ByteString.Char8 as BS
import System.Exit (exitFailure)

testWithTestSequence = do
  (TestSeq.TestSequenceState (_, events',    _), return_value) <- TestSeq.runTestSequence test'
  (TestSeq.TestSequenceState (_, events'',   _), return_value) <- TestSeq.runTestSequence test''
  (TestSeq.TestSequenceState (_, events''',  _), return_value) <- TestSeq.runTestSequence test'''
  (TestSeq.TestSequenceState (_, events'''', _), return_value) <- TestSeq.runTestSequence test''''
  if pattern'    (filt events') &&
     pattern''   (filt events'') &&
     pattern'''  (filt events''') &&
     pattern'''' (filt events'''')
    then do
      putStrLn "Passed TestHashECMWithTestSequence"
      -- printOutEvents events' events'' events''' events''''
      return ()
    else do
      printOutFailedPattern "TestHashECMWithTestSequence.testWithTestSequence"
        (filt events') (filt events'') (filt events''') (filt events'''')
      printOutEvents events' events'' events''' events''''
      exitFailure
  where
    filt = filter someEventsOnly . reverse
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
      testLookups (lookupECM filecache)
    
    test'' = do
      filecache <- newTestECM
        (consistentDuration 100 commonreadnumber) -- Duration between access and expiry time
        1 -- Time check frequency
      testLookups (lookupECM filecache)
            
    test''' = do
      filecache <- newTestECM
        (consistentDuration 1 commonreadnumber) -- Duration between access and expiry time
        12000 -- Time check frequency
      testLookups (lookupECM filecache)
    
    test'''' = do
      filecache <- newTestECM
        (consistentDuration 1 commonreadnumber) -- Duration between access and expiry time
        1 -- Time check frequency
      testLookups (lookupECM filecache)

-- main = testWithTestSequence
