{-# LANGUAGE CPP #-}

-- |
-- Module : Caching.ExpiringCacheMap.Utils.TestSequence
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- TestSequence monad for testing caching behaviour.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Caching.ExpiringCacheMap.HashECM (newECMForM, lookupECM, CacheSettings(..), consistentDuration)
-- > import qualified Caching.ExpiringCacheMap.Utils.TestSequence as TestSeq
-- > 
-- > import qualified Data.ByteString.Char8 as BS
-- > 
-- > test = do
-- >   (TestSeq.TestSequenceState (_, events, _), return_value) <- TestSeq.runTestSequence test'
-- >   (putStrLn . show . reverse) events
-- >   return ()
-- >   where
-- >     test' = do
-- >       filecache <- newECMForM
-- >             (consistentDuration 100 -- Duration between access and expiry time of each item, no state needed.
-- >               (\state _id -> do number <- TestSeq.readNumber
-- >                                 return (state, number)))
-- >             (TestSeq.getCurrentTime >>= return)
-- >             12000 -- Time check frequency: (accumulator `mod` this_number) == 0.
-- >             (CacheWithLRUList 
-- >               6   -- Expected size of key-value map when removing elements.
-- >               6   -- Size of map when to remove items from key-value map.
-- >               12  -- Size of list when to compact
-- >               )
-- >             TestSeq.newTestSVar TestSeq.enterTestSVar TestSeq.readTestSVar
-- >       
-- >       -- Use lookupECM whenever the contents of "file1" is needed.
-- >       b <- lookupECM filecache ("file1" :: BS.ByteString)
-- >       TestSeq.haveNumber b
-- >       b <- lookupECM filecache "file1"
-- >       b <- lookupECM filecache "file2"
-- >       TestSeq.haveNumber b
-- >       return b
-- >
--
-- Evaluating the @test@ function results in a list of events.
--
-- >>> test
-- [GetVar 3,ReadNumber 4,GetTime 7,PutVar 11,HaveNumber 4,GetVar 14,PutVar 17,
--  GetVar 19,ReadNumber 20,GetTime 23,PutVar 27,HaveNumber 20]
-- 
-- In this example the history shows 2 time accesses (@GetTime 7@ and
-- @GetTime 23@) since the time check frequency number is a high value (12000),
-- but regardless the high value a time check is still requested again because
-- of the new key request for @"file2"@.
--
-- Changing the time frequency to 1 will alter the list of events with more
-- frequent time checks:
--
-- >>> test
-- [GetVar 3,ReadNumber 4,GetTime 7,PutVar 11,HaveNumber 4,GetVar 14,GetTime 15,
--  GetTime 18,PutVar 22,GetVar 24,ReadNumber 25,GetTime 28,PutVar 32,
--  HaveNumber 25]
--

module Caching.ExpiringCacheMap.Utils.TestSequence (
    runTestSequence,
    newTestSVar,
    enterTestSVar,
    readTestSVar,
    getCurrentTime,
    readNumber,
    haveNumber,
    TestSequenceEvents(..),
    TestSequenceState(..),
    TestSequence(..),
    TestSVar(..)
) where

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative (Applicative(..))
#endif
import Control.Monad (ap, liftM)
import Data.Word (Word32)

data TestSequenceEvents = 
  GetVar Word32 |
  PutVar Word32 |
  GetTime Word32 |
  ReadNumber Int |
  HaveNumber Int
  deriving (Eq)

instance Show TestSequenceEvents where
  show (GetVar a)     = "GetVar " ++ (show a)
  show (PutVar a)     = "PutVar " ++ (show a)
  show (GetTime a)    = "GetTime " ++ (show a)
  show (ReadNumber a) = "ReadNumber " ++ (show a)
  show (HaveNumber a) = "HaveNumber " ++ (show a)


newtype TestSequenceState b =
  TestSequenceState (Word32, [TestSequenceEvents], Maybe b)
  
instance Show (TestSequenceState ct) where
  show (TestSequenceState (a,b,_)) =
    "TestSequenceState " ++ (show a) ++ " " ++ (show b)

newtype TestSequence b a =
  TestSequence (TestSequenceState b -> (TestSequenceState b, a))

newtype TestSVar a = TestSVar a

-- For GHC 7.10
instance Functor (TestSequence a) where
  fmap = liftM
  
instance Applicative (TestSequence a) where
  pure = return
  (<*>) = ap

instance Monad (TestSequence a) where
  TestSequence fun >>= k =
    TestSequence
      (\state -> let (state', ret) = (fun state)
                     TestSequence fun' = k ret
                  in fun' state')
  return ret = 
    TestSequence $
      \(TestSequenceState (timer, hl, testsvar)) ->
       (TestSequenceState (timer+1,hl, testsvar), ret)

runTestSequence :: Show a => TestSequence b a -> IO (TestSequenceState b, a)
runTestSequence f = do
  let ret = (fun (TestSequenceState (0, [], Nothing)))
   in return ret
  where
    TestSequence fun = (TestSequence
      (\(TestSequenceState (t, hl, testsvar)) ->
        (TestSequenceState (t+1, hl, testsvar), ()))) >> f

newTestSVar :: a -> TestSequence a (TestSVar a)
newTestSVar var = TestSequence $
  \(TestSequenceState (timer, hl, Nothing)) ->
   (TestSequenceState (timer+1, hl, Just var), TestSVar var)

enterTestSVar :: TestSVar a -> (a -> TestSequence a (a,b)) -> TestSequence a b
enterTestSVar testsvar fun = do
  teststate <- readTestSVar testsvar
  (teststate',passalong) <- fun teststate
  putTestSVar testsvar teststate'
  return passalong

-- 'putTestSVar' is used along with 'readTestSVar' to implement enterTestSVar.
--
putTestSVar :: TestSVar a -> a -> TestSequence a a
putTestSVar _testsvar testsvar' = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
   (TestSequenceState (timer+1, (PutVar timer) : hl, Just testsvar'),
      case testsvar of
        Nothing -> testsvar'
        Just testsvar'' -> testsvar'')

readTestSVar :: TestSVar a -> TestSequence a a
readTestSVar _testsvar = TestSequence $
  \(TestSequenceState (timer, hl, Just testsvar)) ->
   (TestSequenceState (timer+1, (GetVar timer) : hl, Just testsvar), testsvar)

getCurrentTime :: TestSequence a Int
getCurrentTime = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
   (TestSequenceState (timer+1, (GetTime timer) : hl, testsvar), fromIntegral timer)

readNumber :: TestSequence a Int
readNumber = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
    let number = fromIntegral timer
     in (TestSequenceState (timer+1, (ReadNumber number) : hl, testsvar), number)

haveNumber :: Int -> TestSequence a ()
haveNumber number = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
   (TestSequenceState (timer+1, (HaveNumber number) : hl, testsvar), ())



