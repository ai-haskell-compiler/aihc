module Pair where

import Math.ExpPairs.Pair (InitPair, initPairToValue, initPairToProjValue)

import Data.Ratio
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Instances ()

testBounds :: InitPair -> Bool
testBounds ip = k>=0 && k<=1%2 && l>=1%2 && l<=1 where
  (k, l) = initPairToValue ip

fracs2proj :: (Rational, Rational) -> (Integer, Integer, Integer)
fracs2proj (q, r) = (k, l, m) where
  dq = denominator q
  dr = denominator r
  m = lcm dq dr
  k = numerator q * (m `div` dq)
  l = numerator r * (m `div` dr)

testProjective :: InitPair -> Bool
testProjective ip = initPairToProjValue ip == fracs2proj (initPairToValue ip)

testSuite :: TestTree
testSuite = testGroup "Pair"
  [ SC.testProperty "bounds" testBounds
  , QC.testProperty "bounds" testBounds
  , SC.testProperty "projective" testProjective
  , QC.testProperty "projective" testProjective
  ]
