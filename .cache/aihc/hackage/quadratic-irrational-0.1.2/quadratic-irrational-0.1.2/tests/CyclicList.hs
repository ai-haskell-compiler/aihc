{-# OPTIONS_GHC -fno-warn-orphans #-}

module CyclicList (tests) where

import Data.Foldable (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary, shrink), (===),
  testProperty)

import Numeric.QuadraticIrrational.CyclicList

instance Arbitrary a => Arbitrary (CycList a) where
  arbitrary = CycList <$> arbitrary <*> arbitrary
  shrink (CycList as bs) = (CycList <$> pure as <*> shrink bs)
                        ++ (CycList <$> shrink as <*> pure bs)

tests :: TestTree
tests =
  testGroup "CyclicList"
    [ testProperty "toList" . withListEquiv $ \asC asL ->
        take 1000 (toList asC) === take 1000 asL
    ]

withListEquiv :: (CycList Integer -> [Integer] -> b) -> CycList Integer -> b
withListEquiv f cl@(CycList as bs) = f cl (as ++ if null bs then [] else cycle bs)
