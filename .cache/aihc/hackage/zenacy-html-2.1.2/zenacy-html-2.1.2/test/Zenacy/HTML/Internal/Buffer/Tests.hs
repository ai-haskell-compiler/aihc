{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Buffer.Tests
  ( testBuffer
  ) where

import Zenacy.HTML.Internal.Buffer
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString as S
import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )

testBuffer :: Test
testBuffer = testGroup "Zenacy.HTML.Internal.Buffer"
  [ testCapacity
  , testSize
  , testReset
  , testPack
  , testApply
  , testTake
  ]

testCapacity :: Test
testCapacity = testCase "buffer capacity" $ do
  assertEqual "TEST 1" (1,100) $
    runST $ do
      b <- bufferNew
      bufferCapacity b
  assertEqual "TEST 2" (1,25600) $
    runST $ do
      b <- bufferNew
      forM_ [1..20000] $ const $ bufferAppend 0x40 b
      bufferCapacity b

testSize :: Test
testSize = testCase "buffer size" $ do
  assertEqual "TEST 1" 0 $
    runST $ do
      b <- bufferNew
      bufferSize b
  assertEqual "TEST 2" 200 $
    runST $ do
      b <- bufferNew
      forM_ [1..200] $ const $ bufferAppend 0x40 b
      bufferSize b
  assertEqual "TEST 3" 1000 $
    runST $ do
      b <- bufferNew
      forM_ [1..1000] $ const $ bufferAppend 0x40 b
      bufferSize b

testReset :: Test
testReset = testCase "buffer reset" $ do
  assertEqual "TEST 1" (0,200,0) $
    runST $ do
      b <- bufferNew
      n0 <- bufferSize b
      forM_ [1..200] $ const $ bufferAppend 0x40 b
      n1 <- bufferSize b
      bufferReset b
      n2 <- bufferSize b
      pure (n0,n1,n2)

testPack :: Test
testPack = testCase "buffer pack" $ do
  assertEqual "TEST 1" x   $ f x
  assertEqual "TEST 2" "0" $ f "0"
  assertEqual "TEST 2" ""  $ f ""
  where
    x = "the quick brown fox jumps over the lazy dog"
    f a =
      runST $ do
        b <- bufferNew
        forM_ (S.unpack a) $ \c -> bufferAppend c b
        bufferPack b

testApply :: Test
testApply = testCase "buffer apply" $ do
  assertEqual "TEST 1" x   $ f x
  assertEqual "TEST 2" "0" $ f "0"
  assertEqual "TEST 2" ""  $ f ""
  where
    x = "the quick brown fox jumps over the lazy dog"
    f a = runST $ do
      b0 <- bufferNew
      b1 <- bufferNew
      forM_ (S.unpack a) $ \c -> bufferAppend c b0
      bufferApply (\w -> bufferAppend w b1) b0
      bufferPack b1

testTake :: Test
testTake = testCase "buffer take" $ do
  assertEqual "TEST 1"
    []
    (f x 0)
  assertEqual "TEST 2"
    [0]
    (f x 1)
  assertEqual "TEST 3"
    [0,1,2,3,4]
    (f x 5)
  assertEqual "TEST 4"
    [0,1,2,3,4,5,6,7,8,9]
    (f x 100)
  where
    x = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09"
    f a n = runST $ do
      b <- bufferNew
      forM_ (S.unpack a) $ \c -> bufferAppend c b
      bufferTake n b
