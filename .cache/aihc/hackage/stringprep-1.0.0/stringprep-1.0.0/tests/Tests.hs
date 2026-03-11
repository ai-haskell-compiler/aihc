{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import qualified Data.Set as Set
import qualified Ranges as R
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
import qualified Text.CharRanges as CR
import qualified Text.StringPrep as SP
import           Unsafe.Coerce (unsafeCoerce)

instance Arbitrary SP.Range where
    arbitrary = oneof [ CR.Single <$> arbitrary
                      , do
                          (x,y) <- (,) <$> arbitrary <*> arbitrary
                          return $ case compare x y of
                              LT -> CR.Range x y
                              EQ -> CR.Single x
                              GT -> CR.Range y x
                      ]
    shrink (CR.Single _) = []
    shrink (CR.Range x y) = [CR.Single x, CR.Single y]

newtype KnownRanges = KR  {unKR :: [CR.Range]} deriving (Show)
newtype RandomRanges = RR {unRR :: [CR.Range]} deriving (Show)

instance Arbitrary KnownRanges where
    arbitrary = KR . concat <$> (listOf1 $ elements spRanges)
    shrink (KR xs) = KR <$> shrink xs

instance Arbitrary RandomRanges where
    arbitrary = RR <$> listOf1 arbitrary
    shrink (RR xs) = RR <$> shrink xs

toRange :: SP.Range -> R.Range Char
toRange (CR.Single x) = R.Single x
toRange (CR.Range x y) = R.Range x y


spRanges = [SP.c11, SP.c12, SP.c21, SP.c22, SP.c3, SP.c4, SP.c5
         , SP.c6, SP.c7, SP.c8, SP.c9, SP.a1]

eqRange :: SP.Range -> R.Range Char -> Bool
eqRange (CR.Range x y) (R.Range x' y') = x == x' && y == y'
eqRange (CR.Single x) (R.Single x') = x == x'
eqRange _ _ = False

rangeSetsEqual :: [SP.Range] -> Bool
rangeSetsEqual rs = eqRanges (Set.toAscList . unsafeCoerce $ CR.toSet rs)
                             (Set.toAscList . R.toSet . R.ranges $ map toRange rs)
  where eqRanges [] [] = True
        eqRanges (x:xs) (y:ys) = eqRange x y && eqRanges xs ys
        eqRanges _ _ = False

prop_knowRangesToSetEqual :: KnownRanges -> Bool
prop_knowRangesToSetEqual (KR rs) = rangeSetsEqual rs

prop_randomRangesToSetEqual :: RandomRanges -> Bool
prop_randomRangesToSetEqual (RR rs) = rangeSetsEqual rs

-- This example came up during testing as a range where the second Single blocked the first one from being merged with the Range in one-pass merging
badRange :: [SP.Range]
badRange = [CR.Single 'v', CR.Single '\234', CR.Range 'g' '\238']

prop_badRangeToSetEqual = rangeSetsEqual badRange

main = $(defaultMainGenerator)
