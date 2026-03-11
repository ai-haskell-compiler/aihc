{-# LANGUAGE ScopedTypeVariables #-}

module KazuraQueueSpec where

import qualified Test.Expectations as T
import qualified Test.KazuraQueue  as T

import qualified Test.Hspec      as HS
import qualified Test.QuickCheck as Q

import qualified Control.Concurrent.KazuraQueue as KQ
import qualified Control.Monad                  as M

import qualified Data.IORef as Ref

import qualified System.Mem.Weak as Weak

writeQueueSpec :: HS.Spec
writeQueueSpec = HS.describe "writeQueue" $ do
    T.whenQueueIsEmpty $ \ prepare -> do
        HS.it "write the value without blocking" . prepare $ \ q -> do
            v :: Int <- Q.generate Q.arbitrary
            KQ.lengthQueue q `T.shouldReturn` 0
            KQ.writeQueue q v `T.shouldNotBlock` 500000
            q `T.queueLengthShouldBeIn` (0, 1)
    T.whenItemsInQueue (1,10) $ \ prepare -> do
        HS.it "write the value without blocking" . prepare $ \ (q, pre) -> do
            let len0 = length pre
            KQ.lengthQueue q `T.shouldReturn` len0
            v :: Int <- Q.generate Q.arbitrary
            KQ.writeQueue q v `T.shouldNotBlock` 500000
            q `T.queueLengthShouldBeIn` (len0, len0 + 1)

readQueueSpec :: HS.Spec
readQueueSpec = HS.describe "readQueue" $ do
    T.whenQueueIsEmpty $ \ prepare -> do
        HS.it "blocks until some one writes item" . prepare $ \ q -> do
            wait <- KQ.readQueue q `T.shouldBlock` 500000
            q `T.queueLengthShouldBeIn` (-1, 0)
            val :: Int <- Q.generate Q.arbitrary
            KQ.writeQueue q val `T.shouldNotBlock` 500000
            r <- wait `T.shouldAwakeFinish` 500000
            r `T.shouldBe` val
            q `T.queueLengthShouldBeIn` (-1, 0)
        HS.it "block and awake out of order (values are in order)" . prepare $ \ q -> do
            waits0 <- M.replicateM 2 $ KQ.readQueue q `T.shouldBlock` 500000
            q `T.queueLengthShouldBeIn` (-2, 0)
            (val1 :: Int, val2) <- Q.generate Q.arbitrary

            KQ.writeQueue q val1 `T.shouldNotBlock` 500000
            (r1, waits1) <- waits0 `T.onlyOneShouldAwakeFinish` 500000
            q `T.queueLengthShouldBeIn` (-2, 0)

            KQ.writeQueue q val2 `T.shouldNotBlock` 500000
            (r2, _)      <- waits1 `T.onlyOneShouldAwakeFinish` 500000
            q `T.queueLengthShouldBeIn` (-2, 0)

            (r1, r2) `T.shouldBe` (val1, val2)
    T.whenQueueIsEmpty $ \ prepare -> do
        HS.it "the item in a Queue is not evaluated by write/read" . prepare $ \ q -> do
            KQ.writeQueue q ([1..] :: [Int]) `T.shouldNotBlock` 500000
            M.void $ KQ.readQueue q `T.shouldNotBlock` 500000
    T.whenQueueIsEmpty $ \ prepare -> do
        HS.it "the item in a Queue can be garbage collected after read" . prepare $ \ q -> do
            ref <- Ref.newIORef True
            weak <- Weak.mkWeakPtr ref Nothing
            KQ.writeQueue q ref `T.shouldNotBlock` 500000
            T.shouldNotBeGarbageCollected weak
            M.void $ KQ.readQueue q `T.shouldNotBlock` 500000
            T.shouldBeGarbageCollected weak
    T.whenItemsInQueue (1,10) $ \ prepare -> do
        HS.it "read one value without blocking" . prepare $ \ (q, pre) -> do
            r :: Int <- KQ.readQueue q `T.shouldNotBlock` 500000
            r `T.shouldBe` head pre

tryReadQueueSpec :: HS.Spec
tryReadQueueSpec = HS.describe "tryReadQueue" $ do
    T.whenQueueIsEmpty $ \ prepare -> do
        HS.it "immediately returns without reading value" . prepare $ \ q -> do
            mret1 <- KQ.tryReadQueue q `T.shouldNotBlock` 500000
            mret1 `T.shouldBe` Nothing
            q `T.queueLengthShouldBeIn` (-1, 0)
            wait <- KQ.readQueue q `T.shouldBlock` 500000
            q `T.queueLengthShouldBeIn` (-2, 0)
            mret2 <- KQ.tryReadQueue q `T.shouldNotBlock` 500000
            mret2 `T.shouldBe` Nothing
            q `T.queueLengthShouldBeIn` (-3, 0)
            val :: Int <- Q.generate Q.arbitrary
            KQ.writeQueue q val `T.shouldNotBlock` 500000
            r <- wait `T.shouldAwakeFinish` 500000
            r `T.shouldBe` val
            q `T.queueLengthShouldBeIn` (-3, 0)
            mret3 <- KQ.tryReadQueue q `T.shouldNotBlock` 500000
            mret3 `T.shouldBe` Nothing
            q `T.queueLengthShouldBeIn` (-4, 0)
        HS.it "read value after writing" . prepare $ \ q -> do
            (val1 :: Int, val2) <- Q.generate Q.arbitrary
            KQ.writeQueue q val1 `T.shouldNotBlock` 500000
            mret1 <- KQ.tryReadQueue q `T.shouldNotBlock` 500000
            mret1 `T.shouldBe` Just val1
            KQ.writeQueue q val2 `T.shouldNotBlock` 500000
            mret2 <- KQ.tryReadQueue q `T.shouldNotBlock` 500000
            mret2 `T.shouldBe` Just val2
    T.whenItemsInQueue (1,10) $ \ prepare -> do
        HS.it "read one value without blocking" . prepare $ \ (q, pre) -> do
            r :: Maybe Int <- KQ.tryReadQueue q `T.shouldNotBlock` 500000
            r `T.shouldBe` Just (head pre)

spec :: HS.Spec
spec = HS.describe "KazuraQueue basic specs" $ do
    writeQueueSpec
    readQueueSpec
    tryReadQueueSpec

