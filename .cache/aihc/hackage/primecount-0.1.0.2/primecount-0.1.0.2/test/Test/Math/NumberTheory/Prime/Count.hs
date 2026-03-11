-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
module Test.Math.NumberTheory.Prime.Count (tests) where

import Control.Exception (SomeException, catch, evaluate)
import Data.Int (Int64)
import Math.NumberTheory.Prime.Count
  ( nthPrime,
    nthPrimeMaxBound,
    primePhi,
    primePi,
    primePiMaxBound,
  )
import System.IO (stderr)
import System.IO.Silently (hSilence)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Prime.Count"
    [primePiTest, nthPrimeTest, primePhiTest]

throwsException :: a -> IO Bool
throwsException thunk =
  (hSilence [stderr] (evaluate thunk) >> pure False)
    `catch` (\(_ :: SomeException) -> pure True)

primePiTest :: TestTree
primePiTest =
  testGroup
    "primePi tests"
    [ testCase "works for negative inputs" $
        primePi (-1 :: Int) @?= 0,
      testCase "works for small inputs" $ do
        primePi (5 :: Int) @?= 3
        primePi (10 :: Int) @?= 4
        primePi (100 :: Int) @?= 25,
      testCase "throws error when input is too large" $
        let tooBig :: Integer
            tooBig = primePiMaxBound + 1
         in throwsException (primePi tooBig) >>= (@?= True)
    ]

nthPrimeTest :: TestTree
nthPrimeTest =
  testGroup
    "nthPrime"
    [ testCase "throws error for non-positive inputs" $ do
        throwsException (nthPrime (0 :: Int)) >>= (@?= True)
        throwsException (nthPrime ((-1) :: Int)) >>= (@?= True),
      testCase "works for small inputs" $ do
        nthPrime (1 :: Int) @?= 2
        nthPrime (5 :: Int) @?= 11
        nthPrime (25 :: Int) @?= 97,
      testCase "throws error when input is too large" $
        let tooBig :: Integer
            tooBig = nthPrimeMaxBound + 1
         in throwsException (nthPrime tooBig) >>= (@?= True)
    ]

primePhiTest :: TestTree
primePhiTest =
  testGroup
    "primePhi"
    [ testCase "is 0 if n is not positive" $ do
        primePhi (0 :: Int) undefined @?= 0
        primePhi ((-1) :: Int) undefined @?= 0,
      testCase "is n if a is not positive" $ do
        primePhi (10 :: Int) 0 @?= 10
        primePhi (10 :: Int) (-1) @?= 10,
      testCase "works for small inputs" $ do
        primePhi (1 :: Int) 2 @?= 1
        primePhi (10 :: Int) 2 @?= 3
        primePhi (10 :: Int) 3 @?= 2
        primePhi (10 :: Int) 4 @?= 1
        primePhi (10 :: Int) 5 @?= 1,
      testCase "throws error if n is too large" $ do
        throwsException (primePhi tooBig 2) >>= (@?= True)
        throwsException (primePhi tooBig 3) >>= (@?= True),
      testCase "works even if a is too large" $
        primePhi 10 tooBig @?= 1
    ]
  where
    tooBig :: Integer
    tooBig = toInteger (maxBound :: Int64) + 1
