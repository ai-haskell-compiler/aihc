{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- This is only okay in test classes

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import System.Random

import Data.Range
import qualified Data.Range.Algebra as Alg

import Test.RangeMerge

data UnequalPair a = UnequalPair (a, a)
   deriving (Show)

instance (Integral a, Num a, Eq a) => Arbitrary (UnequalPair a) where
   arbitrary = do
      first <- arbitrarySizedIntegral
      second <- arbitrarySizedIntegral `suchThat` (/= first)
      return $ UnequalPair (first, second)

prop_singleton_in_range :: Integer -> Bool
prop_singleton_in_range a = inRange (SingletonRange a) a

prop_singleton_not_in_range :: (Ord a) => UnequalPair a -> Bool
prop_singleton_not_in_range (UnequalPair (first, second)) = not $ inRange (SingletonRange first) second

data SpanContains a = SpanContains (a, a) a
   deriving (Show)

instance (Num a, Integral a, Ord a, Random a) => Arbitrary (SpanContains a) where
   arbitrary = do
      begin <- arbitrarySizedIntegral
      end <- arbitrarySizedIntegral `suchThat` (>= begin)
      middle <- choose (begin, end)
      return $ SpanContains (begin, end) middle

prop_span_contains :: SpanContains Integer -> Bool
prop_span_contains (SpanContains (begin, end) middle) = inRange (SpanRange (Bound begin Inclusive) (Bound end Inclusive)) middle

prop_infinite_range_contains_everything :: Integer -> Bool
prop_infinite_range_contains_everything = inRange InfiniteRange

tests_inRange = testGroup "inRange Function"
   [ testProperty "equal singletons in range" prop_singleton_in_range
   , testProperty "unequal singletons not in range" prop_singleton_not_in_range
   , testProperty "spans contain values in their middles" prop_span_contains
   , testProperty "infinite ranges contain everything" prop_infinite_range_contains_everything
   ]

instance (Num a, Integral a, Ord a, Enum a) => Arbitrary (Range a) where
   arbitrary = oneof
      [ generateSingleton
      , generateSpan
      , generateLowerBound
      , generateUpperBound
      , generateInfiniteRange
      ]
      where
         generateSingleton = liftM SingletonRange arbitrarySizedIntegral
         generateSpan = do
            first <- arbitrarySizedIntegral
            second <- arbitrarySizedIntegral `suchThat` (> first)
            return $ first +=+ second
         generateLowerBound = liftM lbi arbitrarySizedIntegral
         generateUpperBound = liftM ubi arbitrarySizedIntegral
         generateInfiniteRange :: Gen (Range a)
         generateInfiniteRange = return InfiniteRange

-- an intersection of a value followed by a union of that value should be the identity.
-- This is false. An intersection of a value followed by a union of that value should be
-- the value itself.
-- (1, 3) union (3, 4) => (1, 4)
-- (1, 3) intersection (3, 4) = (3, 3)
-- ((1, 3) intersection (3, 4)) union (3, 4) => (3, 4)

prop_in_range_out_of_range_after_invert :: (Integer, [Range Integer]) -> Bool
prop_in_range_out_of_range_after_invert (point, ranges) =
   (inRanges ranges point) /= (inRanges (invert ranges) point)

test_ranges_invert = testGroup "invert function for ranges"
   [ testProperty "element in range is now out of range after invert" prop_in_range_out_of_range_after_invert
   ]

instance (Num a, Integral a, Ord a, Enum a) => Arbitrary (Alg.RangeExpr [Range a]) where
  arbitrary = frequency
    [ (3, Alg.const <$> arbitrary)
    , (1, Alg.invert <$> arbitrary)
    , (1, Alg.union <$> arbitrary <*> arbitrary)
    , (1, Alg.intersection <$> arbitrary <*> arbitrary)
    , (1, Alg.difference <$> arbitrary <*> arbitrary)
    ]

prop_equivalence_eval_and_evalPredicate :: ([Integer], Alg.RangeExpr [Range Integer]) -> Bool
prop_equivalence_eval_and_evalPredicate (points, expr) = actual == expected
  where
      actual = map (inRanges $ Alg.eval expr) points
      expected = map (Alg.eval $ fmap inRanges expr) points

test_algebra_equivalence = testGroup "algebra equivalence"
   [ testProperty "eval and evalPredicate" prop_equivalence_eval_and_evalPredicate
   ]

--tests :: [Test]
tests =
   [ tests_inRange
   , test_ranges_invert
   , test_algebra_equivalence
   ]
   ++ rangeMergeTestCases

main = defaultMain tests
