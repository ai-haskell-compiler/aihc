{-# LANGUAGE
    ScopedTypeVariables
  , RankNTypes
  #-}

module Main where

import Data.Map.Strict as Map
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM.TMapMVar as TMapMVar
import Control.Concurrent.STM.TVar (readTVar, newTVarIO, writeTVar, modifyTVar')
import Control.Concurrent.STM.TMVar (tryReadTMVar)
import Control.Concurrent.STM (atomically, STM)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary (..), Property, ioProperty)
import System.IO.Unsafe (unsafePerformIO)



main :: IO ()
main =
  defaultMain $ testGroup "Control.Concurrent.STM.TMapMVar"
    [ testProperty "Insert in Empty" insertInEmpty
    , testProperty "Insert in Full" insertInFull
    , testProperty "Insert Force doesn't Block" insertForceDoesntBlock
    , testProperty "Empty after Lookup" emptyAfterLookup
    , testProperty "Observe doesn't Mutate" nonMutativeObserve
    , testProperty "Lookup before Insert" lookupThenInsert
    ]


insertInEmpty :: Int -> Int -> Property
insertInEmpty k v = ioProperty $ do
  var <- atomically newTMapMVar
  v' <- atomically $ do
    TMapMVar.insert var k v
    TMapMVar.observe var k
  pure (v == v')


insertInFull :: Int -> Int -> Property
insertInFull k v = ioProperty $ do
  var <- do
    m <- atomically newTMapMVar
    atomically $ TMapMVar.insert m k (v-1)
    pure m
  async $ do
    threadDelay 10
    atomically $ TMapMVar.delete var k
  v' <- atomically $ do
    TMapMVar.insert var k v
    TMapMVar.lookup var k
  pure (v == v')


insertForceDoesntBlock :: Int -> Int -> Int -> Property
insertForceDoesntBlock k v1 v2 = ioProperty $ do
  var <- atomically newTMapMVar
  v' <- atomically $ do
    TMapMVar.insert var k v1
    TMapMVar.insertForce var k v2
    TMapMVar.lookup var k
  pure (v' == v2)


emptyAfterLookup :: Int -> Int -> Property
emptyAfterLookup k v = ioProperty $ do
  var <- atomically newTMapMVar
  mV' <- atomically $ do
    TMapMVar.insert var k v
    TMapMVar.delete var k
    TMapMVar.tryObserve var k
  pure (mV' == Nothing)


nonMutativeObserve :: Int -> Int -> Property
nonMutativeObserve k v = ioProperty $ do
  var <- atomically newTMapMVar
  (v1,v2) <- atomically $ do
    TMapMVar.insert var k v
    v1' <- TMapMVar.observe var k
    v2' <- TMapMVar.observe var k
    pure (v1',v2')
  pure (v1 == v2 && v2 == v)


lookupThenInsert :: Int -> Int -> Property
lookupThenInsert k v = ioProperty $ do
  var <- atomically newTMapMVar
  async $ do
    threadDelay 10
    atomically $ TMapMVar.insert var k v
  v' <- atomically $ TMapMVar.lookup var k
  pure (v' == v)
