module Kratzel where

import Data.Ratio
import Math.ExpPairs
import Math.ExpPairs.Kratzel

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC (testProperty)
import Test.Tasty.HUnit

import Instances
import Etalon (testEtalon)

testAbMonotonic :: Sorted (Positive Integer, Positive Integer, Positive Integer, Positive Integer) -> Bool
testAbMonotonic (Sorted (Positive a, Positive c, Positive b, Positive d))
  = (a == c && b == d) || zab > zcd
    where
      zab = optimalValue $ snd $ tauab a b
      zcd = optimalValue $ snd $ tauab c d

testAbCompareLow :: Sorted (Positive Integer, Positive Integer) -> Bool
testAbCompareLow (Sorted (Positive a, Positive b))
  = optimalValue (snd $ tauab a b) >= Finite (1 % (2 * a + 2 * b))

testAbCompareHigh :: Sorted (Positive Integer, Positive Integer) -> Bool
testAbCompareHigh (Sorted (Positive a, Positive b))
  = optimalValue (snd $ tauab a b) < Finite (1 % (a + b))

testAbcMonotonic :: Sorted (Positive Integer, Positive Integer, Positive Integer, Positive Integer, Positive Integer, Positive Integer) -> Bool
testAbcMonotonic (Sorted (Positive a, Positive d, Positive b, Positive e, Positive c, Positive f))
  = (a == d && b == e && c == f) || theoremAbc `elem` [Kolesnik, Kr64, Kr65] || zabc >= zdef
    where
      (theoremAbc, resultAbc) = tauabc a b c
      zabc = optimalValue resultAbc
      zdef = optimalValue $ snd $ tauabc d e f

testAbcCompareLow :: Sorted (Positive Integer, Positive Integer, Positive Integer) -> Bool
testAbcCompareLow (Sorted (Positive a, Positive b, Positive c))
  = optimalValue (snd $ tauabc a b c) >= Finite (1 % (a + b + c))

testAbcCompareHigh :: Sorted (Positive Integer, Positive Integer, Positive Integer) -> Bool
testAbcCompareHigh (Sorted (Positive a, Positive b, Positive c))
  = c >= a + b || optimalValue (snd $ tauabc a b c) < Finite (2 % (a + b + c))

testAbcdMonotonic :: Sorted (Positive Integer, Positive Integer, Positive Integer, Positive Integer, Positive Integer, Positive Integer, Positive Integer, Positive Integer) -> Bool
testAbcdMonotonic (Sorted (Positive a, Positive e, Positive b, Positive f, Positive c, Positive g, Positive d, Positive h))
  = (a == e && b == f && c == g && d == h) || theoremAbcd `elem` [HeathBrown, Kr1992_32] || zabcd >= zefgh
    where
      (theoremAbcd, resultAbcd) = tauabcd a b c d
      zabcd = optimalValue resultAbcd
      zefgh = optimalValue $ snd $ tauabcd e f g h

testAbcdCompareLow :: Sorted (Positive Integer, Positive Integer, Positive Integer, Positive Integer) -> Bool
testAbcdCompareLow (Sorted (Positive a, Positive b, Positive c, Positive d))
  = optimalValue (snd $ tauabcd a b c d) >= Finite (1 % (a + b + c + d))

-- | Kratzel1988, Eq. (6.31)
testAbcdCompareHigh :: Sorted (Positive Integer, Positive Integer, Positive Integer, Positive Integer) -> Bool
testAbcdCompareHigh (Sorted (Positive a, Positive b, Positive c, Positive d))
  = d >= a + b + c || optimalValue (snd $ tauabcd a b c d) <= Finite ((a + b + c) % (a * (a + b + c + d)))

etalonTauab :: Integer -> Integer -> Integer -> Integer -> Bool
etalonTauab a b c d = Finite (c % d) >= (optimalValue . snd) (tauab a b)

etalonTauabc :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
etalonTauabc a b c d e = Finite (d % e) >= (optimalValue . snd) (tauabc a b c)

testSuite :: TestTree
testSuite = testGroup "Kratzel"
  [ testCase "etalon tauab"
    (testEtalon 100 (\[x1, x2, x3, x4] -> etalonTauab x1 x2 x3 x4) "tests/etalon-tauab.txt")
  , testCase "etalon tauabc"
    (testEtalon 100 (\[x1, x2, x3, x4, x5] -> etalonTauabc x1 x2 x3 x4 x5) "tests/etalon-tauabc.txt")

  , SC.testProperty "tauabcd compare with 1/(a+b+c+d)" testAbcdCompareLow
  , QC.testProperty "tauabcd compare with 1/(a+b+c+d)" testAbcdCompareLow
  , SC.testProperty "tauabcd compare with (6.31)" testAbcdCompareHigh
  , QC.testProperty "tauabcd compare with (6.31)" testAbcdCompareHigh
  , QC.testProperty "tauabcd monotonic" testAbcdMonotonic

  , SC.testProperty "tauabc compare with 1/(a+b+c)" testAbcCompareLow
  , QC.testProperty "tauabc compare with 1/(a+b+c)" testAbcCompareLow
  , SC.testProperty "tauabc compare with 2/(a+b+c)" testAbcCompareHigh
  , QC.testProperty "tauabc compare with 2/(a+b+c)" testAbcCompareHigh
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 3)) $
      SC.testProperty "tauabc monotonic" testAbcMonotonic
  , QC.testProperty "tauabc monotonic" testAbcMonotonic

  , SC.testProperty "tauab compare with 1/2(a+b)" testAbCompareLow
  , QC.testProperty "tauab compare with 1/2(a+b)" testAbCompareLow
  , SC.testProperty "tauab compare with 1/(a+b)" testAbCompareHigh
  , QC.testProperty "tauab compare with 1/(a+b)" testAbCompareHigh
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "tauab monotonic" testAbMonotonic
  , QC.testProperty "tauab monotonic" testAbMonotonic
  ]
