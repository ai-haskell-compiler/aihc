module MenzerNowak where

import Data.Ratio
import Math.ExpPairs
import Math.ExpPairs.MenzerNowak
import Math.ExpPairs.Kratzel

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC (testProperty)

import Instances

testMonotonic :: Sorted (Positive Integer, Positive Integer, Positive Integer, Positive Integer) -> Bool
testMonotonic (Sorted (Positive a, Positive c, Positive b, Positive d))
  =  (a == c && b == d) || zab > zcd
    where
      zab = optimalValue $ menzerNowak a b
      zcd = optimalValue $ menzerNowak c d

testCompareLow :: Sorted (Positive Integer, Positive Integer) -> Bool
testCompareLow (Sorted (Positive a, Positive b))
  = optimalValue (snd $ tauab a b) <= optimalValue (menzerNowak a b) + Finite eps
    where
      eps = 1 % (10 ^ (30::Integer))

testCompareHigh :: Sorted (Positive Integer, Positive Integer) -> Bool
testCompareHigh (Sorted (Positive a, Positive b))
  = optimalValue (menzerNowak a b) < 1

testSuite :: TestTree
testSuite = testGroup "MenzerNowak"
  [ SC.testProperty "compare with tauab" testCompareLow
  , QC.testProperty "compare with tauab" testCompareLow
  , SC.testProperty "compare with 1" testCompareHigh
  , QC.testProperty "compare with 1" testCompareHigh
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "monotonic" testMonotonic
  , QC.testProperty "monotonic" testMonotonic
  ]
