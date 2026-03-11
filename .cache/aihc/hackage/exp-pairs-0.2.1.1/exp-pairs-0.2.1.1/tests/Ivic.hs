module Ivic where

import Data.Ratio
import Math.ExpPairs
import Math.ExpPairs.Ivic

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC (testProperty, QuickCheckTests(..))
import Test.Tasty.HUnit

import Debug.Trace

import Instances
import Etalon (testEtalon)

fromMinus3To3 :: Rational -> Rational
fromMinus3To3 n = (n - 1 % 2) * 6

fromHalfToOne :: Rational -> Rational
fromHalfToOne n = n / 2 + 1 % 2

testZetaOnS1 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testZetaOnS1 (Sorted (Ratio01 a', Ratio01 b')) = a == b || za >= zb where
  [ a,  b] = map fromMinus3To3 [a', b']
  [za, zb] = map (optimalValue . zetaOnS) [a, b]

-- Strict comparison without 3e-4 may fail due to the granularity of 'sect'.
testZetaOnS2 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testZetaOnS2 (Sorted (Ratio01 a, Ratio01 b)) = a == b || za + 5e-4 > zb where
  [za, zb] = map (optimalValue . zetaOnS) [a, b]

testZetaOnSsym :: Ratio01 Rational -> Bool
testZetaOnSsym (Ratio01 a') = (toRational . abs) (za - za') == abs (a - 1 % 2) where
  a   = fromMinus3To3 a'
  za  = optimalValue $ zetaOnS a
  za' = optimalValue $ zetaOnS (1 - a)

testZetaOnSZero :: Ratio01 Rational -> Bool
testZetaOnSZero (Ratio01 a') = a < 1 || optimalValue (zetaOnS a) == 0 where
  a = fromMinus3To3 a'

testMOnS1 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testMOnS1 (Sorted (Ratio01 a', Ratio01 b')) = a == b || za <= zb where
  [ a,  b] = map fromMinus3To3 [a', b']
  [za, zb] = map (optimalValue . mOnS) [a, b]

testMOnS2 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testMOnS2 (Sorted (Ratio01 a', Ratio01 b')) = a == b || za < zb where
  [ a,  b] = map fromHalfToOne [a', b']
  [za, zb] = map (optimalValue . mOnS) [a, b]

testMOnSZero :: Ratio01 Rational -> Bool
testMOnSZero (Ratio01 a') = a >= 1%2 || (optimalValue . mOnS) a == 0 where
  a = fromMinus3To3 a'

testMOnSInf :: Ratio01 Rational -> Bool
testMOnSInf (Ratio01 a') = a < 1 || (optimalValue . mOnS) a == InfPlus where
  a = fromMinus3To3 a'

testZetaReverse1 :: Ratio01 Rational -> Bool
testZetaReverse1 (Ratio01 s') = t <= s + 2.1e-2 && s <= t + 2e-3 ||
  trace (show $ fromRational $ s-t) False
  where
    s = fromHalfToOne s'
    zs = zetaOnS s
    t = toRational $ optimalValue $ reverseZetaOnS $ toRational $ optimalValue zs

testZetaReverse2 :: Ratio01 Rational -> Bool
testZetaReverse2 (Ratio01 s') = t <= s + 1e-10 && s <= t + 4e-3 ||
  trace (show $ fromRational $ s-t) False
  where
    s = s' * 32 / 205
    zs = reverseZetaOnS s
    t = toRational $ optimalValue $ zetaOnS $ toRational $ optimalValue zs

testMOnSReverse1 :: Ratio01 Rational -> Bool
testMOnSReverse1 (Ratio01 s') = t <= s + 4e-2 && s <= t + 1.4e-3 ||
  trace (show $ fromRational $ s-t) False
  where
    s = fromHalfToOne s'
    zs = mOnS s
    t = toRational $ reverseMOnS 1e-3 $ optimalValue zs

testMOnSReverse2 :: Ratio01 Rational -> Bool
testMOnSReverse2 (Ratio01 s')
  =  s' == 0
  || t' == InfPlus || t' == InfMinus
  || recip t <= recip s + 1e-3 && recip s <= recip t + 1e-3
  || trace (show $ fromRational $ recip s - recip t) False
    where
      s  = 4 * recip s'
      zs = reverseMOnS 1e-3 (Finite s)
      t' = optimalValue $ mOnS $ toRational zs
      t  = toRational t'

testMBigOnHalfReverse1 :: Positive Rational -> Bool
testMBigOnHalfReverse1 (Positive s') = recip t <= recip s + 2e-3 && recip s <= recip t + 1e-10 ||
  trace (show $ fromRational $ recip s - recip t) False
  where
    s = s' + 4
    zs = mBigOnHalf s
    t = toRational $ optimalValue $ reverseMBigOnHalf $ toRational $ optimalValue zs

testMBigOnHalfReverse2 :: Positive Rational -> Bool
testMBigOnHalfReverse2 (Positive s') = recip t <= recip s + 2e-3 && recip s <= recip t + 1e-10 ||
  trace (show $ fromRational $ recip s - recip t) False
  where
    s = s' + 1
    zs = reverseMBigOnHalf s
    t = toRational $ optimalValue $ mBigOnHalf $ toRational $ optimalValue zs

etalonZetaOnS :: Integer -> Integer -> Integer -> Integer -> Bool
etalonZetaOnS a b c d = Finite (c%d) >= optimalValue (zetaOnS $ a%b)

etalonMOnS :: Integer -> Integer -> Integer -> Integer -> Bool
etalonMOnS a b c d = Finite (c%d) <= (optimalValue . mOnS) (a%b)

testSuite :: TestTree
testSuite = testGroup "Ivic"
  [ testCase "etalon zetaOnS"
    (testEtalon 100 (\(a:b:c:d:_) -> etalonZetaOnS a b c d) "tests/etalon-zetaOnS.txt")
  , testCase "etalon mOnS"
    (testEtalon 100 (\(a:b:c:d:_) -> etalonMOnS a b c d) "tests/etalon-mOnS.txt")
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "zetaOnS monotonic" testZetaOnS1
  , QC.testProperty "zetaOnS monotonic" testZetaOnS1
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "zetaOnS strict monotonic" testZetaOnS2
  , QC.testProperty "zetaOnS strict monotonic" testZetaOnS2
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "mOnS monotonic" testMOnS1
  , QC.testProperty "mOnS monotonic" testMOnS1
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "mOnS strict monotonic" testMOnS2
  , QC.testProperty "mOnS strict monotonic" testMOnS2

  , SC.testProperty "reverseZetaOnS . zetaOnS == id" testZetaReverse1
  , QC.testProperty "reverseZetaOnS . zetaOnS == id" testZetaReverse1
  , SC.testProperty "zetaOnS . reverseZetaOnS == id" testZetaReverse2
  , QC.testProperty "zetaOnS . reverseZetaOnS == id" testZetaReverse2

  , SC.testProperty "reverseMOnS . mOnS == id" testMOnSReverse1
  , adjustOption (\(QC.QuickCheckTests n) -> QC.QuickCheckTests (n `min` 100)) $
      QC.testProperty "reverseMOnS . mOnS == id" testMOnSReverse1
  , SC.testProperty "mOnS . reverseMOnS == id" testMOnSReverse2
  , adjustOption (\(QC.QuickCheckTests n) -> QC.QuickCheckTests (n `min` 100)) $
      QC.testProperty "mOnS . reverseMOnS == id" testMOnSReverse2

  , SC.testProperty "reverseMBigOnHalf . mBigOnHalf == id" testMBigOnHalfReverse1
  , QC.testProperty "reverseMBigOnHalf . mBigOnHalf == id" testMBigOnHalfReverse1
  , SC.testProperty "mBigOnHalf . reverseMBigOnHalf == id" testMBigOnHalfReverse2
  , QC.testProperty "mBigOnHalf . reverseMBigOnHalf == id" testMBigOnHalfReverse2

  , SC.testProperty "zetaOnS symmetry" testZetaOnSsym
  , QC.testProperty "zetaOnS symmetry" testZetaOnSsym
  , SC.testProperty "zetaOnS above s=1" testZetaOnSZero
  , QC.testProperty "zetaOnS above s=1" testZetaOnSZero
  , SC.testProperty "mOnS below s=1/2" testMOnSZero
  , QC.testProperty "mOnS below s=1/2" testMOnSZero
  , SC.testProperty "mOnS above s=1" testMOnSInf
  , QC.testProperty "mOnS above s=1" testMOnSInf
  ]


