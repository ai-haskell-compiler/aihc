-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}

module Test.Data.List.ApplyMerge (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Data.Complex (Complex ((:+)))
import Data.List (sortOn)
import Data.List.ApplyMerge (applyMergeBy, applyMergeOn)
import Data.Ratio ((%))
import Test.ApplyMerge.Common (basicTest, blockTest, maxTest, skewedTest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge"
    [ testGroup
        "applyMergeOn"
        [ basicTest (applyMergeOn id),
          skewedTest (applyMergeOn id),
          blockTest (applyMergeOn id),
          maxTest (applyMergeOn id),
          gaussianIntegerTest
        ],
      testGroup
        "applyMergeBy"
        [ basicTest (applyMergeBy compare),
          skewedTest (applyMergeBy compare),
          blockTest (applyMergeBy compare),
          maxTest (applyMergeBy compare)
        ]
    ]

gaussianIntegerTest :: TestTree
gaussianIntegerTest =
  testCase "gaussian integers x + yi, with 0 <= x <= y, ordered by norm" $ do
    let actual =
          take 100 $
            applyMergeOn
              (\x -> (norm x, slope x))
              (\x k -> x :+ (x + k))
              [0 ..]
              [0 ..]
        expected =
          take 100 $
            filter (\(x :+ y) -> x <= y) $
              sortOn
                (\x -> (norm x, slope x))
                (liftA2 (:+) [0 .. 100] [0 .. 100])
     in actual @?= expected

norm :: Complex Integer -> Integer
norm (a :+ b) = a * a + b * b

data Slope = Finite Rational | Infinity
  deriving (Eq, Show, Ord)

slope :: Complex Integer -> Slope
slope (x :+ y) =
  if x == 0
    then Infinity
    else Finite (y % x)
