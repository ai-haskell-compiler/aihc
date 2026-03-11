{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module LinearForm where

import Data.Ratio

import Math.ExpPairs.LinearForm
import Math.ExpPairs.RatioInf

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Instances ()

extractCoeffs :: Num t => LinearForm t -> (t, t, t)
extractCoeffs lf =
  ( evalLF (1, 0, 0) lf
  , evalLF (0, 1, 0) lf
  , evalLF (0, 0, 1) lf
  )

testPlus :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
testPlus a b c d e f = a+d==ad && b+e==be && c+f==cf where
  l1 = LinearForm a b c
  l2 = LinearForm d e f
  (ad, be, cf) = extractCoeffs (l1 + l2)

testMinus :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
testMinus a b c d e f = a-d==ad && b-e==be && c-f==cf where
  l1 = LinearForm a b c
  l2 = LinearForm d e f
  (ad, be, cf) = extractCoeffs (l1 - l2)

testFromInteger :: Integer -> Bool
testFromInteger a = evalLF (0, 0, 1) (fromInteger a) == a

testSubstitute1 :: LinearForm Rational -> Bool
testSubstitute1 a
  =  substituteLF (a, 0, 0) (LinearForm 1 0 0) == a
  && substituteLF (0, a, 0) (LinearForm 0 1 0) == a
  && substituteLF (0, 0, a) (LinearForm 0 0 1) == a

testSubstitute2 :: LinearForm Rational -> LinearForm Rational
                -> LinearForm Rational -> LinearForm Rational
                -> LinearForm Rational -> LinearForm Rational
                -> LinearForm Rational -> Bool
testSubstitute2 a1 a2 b1 b2 c1 c2 lf
  =  substituteLF (a1 + a2, b1 + b2, c1 + c2) lf
  == substituteLF (a1, b1, c1) lf + substituteLF (a2, b2, c2) lf

testNegateRF :: RationalForm Rational -> Integer -> Integer -> Integer -> Bool
testNegateRF rf k l m = case evalRF (k, l, m) rf of
  x@Finite{} -> x == negate (evalRF (k, l, m) (negate rf))
  _          -> True

testNegateVarsRF :: RationalForm Rational -> Integer -> Integer -> Integer -> Bool
testNegateVarsRF rf k l m =
  evalRF (k, l, m) rf == evalRF (-k, -l, -m) rf

testFromIntegerRF :: Integer -> Bool
testFromIntegerRF a = evalRF (0, 0, 1) (fromInteger a) == Finite (a % 1)

testCheckConstraint :: Integer -> Integer -> Integer -> Constraint Rational -> Bool
testCheckConstraint k l m c@(Constraint lf ineq)
  =  (ineq==Strict    && isZero || x || y)
  && (ineq==NonStrict && isZero || not (x && y))
  where
    x = checkConstraint (k, l, m) c
    y = checkConstraint (k, l, m) (Constraint (negate lf) ineq)
    isZero = evalLF (fromInteger k, fromInteger l, fromInteger m) lf == 0

testSuite :: TestTree
testSuite = testGroup "LinearForm"
  [ QC.testProperty "plus" testPlus
  , QC.testProperty "minus" testMinus
  , SC.testProperty "from integer LF" testFromInteger
  , QC.testProperty "from integer LF" testFromInteger
  , QC.testProperty "substitute component" testSubstitute1
  , QC.testProperty "substitution is linear" testSubstitute2
  , QC.testProperty "negate RF" testNegateRF
  , QC.testProperty "negate vars RF" testNegateVarsRF
  , SC.testProperty "from integer RF" testFromIntegerRF
  , QC.testProperty "from integer RF" testFromIntegerRF
  , QC.testProperty "constraint" testCheckConstraint
  ]
