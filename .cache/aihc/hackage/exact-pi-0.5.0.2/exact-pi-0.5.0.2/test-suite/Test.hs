{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Data.Fixed            (Fixed(..))
import Data.Ratio            ((%))
import Test.Tasty            (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit      ((@?=), Assertion, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck       (Positive(..))

import Data.ExactPi
import TestUtils             (E, getValue, getDigit, getDigitBBP)

-- test pi^2 first since it does not rely on square roots
piSquaredDouble :: Assertion
piSquaredDouble = getValue (Exact 2 1) @?= (pi^2 :: Double)

-- first 57 digits of pi^2
-- http://www.wolframalpha.com/input/?i=pi%5E2
piSquaredWAstart :: Assertion
piSquaredWAstart = getValue (Exact 2 1) @?= piSquared

piSquared :: Fixed (E 57)
piSquared = 9.869604401089358618834490999876151135313699407240790626413

-- last 21 digits of pi^2 on wolfram alpha http://www.wolframalpha.com/input/?i=pi%5E2
-- by asking for more digits as much as possible
piSquaredWAend :: Assertion
piSquaredWAend = x `mod` (10^21) @?= 643271910414561208753
  where
    MkFixed x = getValue (Exact 2 1) :: Fixed (E 3647)

-- test first term matches formula of chudnovsky's algorithm
firstApproximation :: Assertion
firstApproximation = head (rationalApproximations (Exact 2 1)) @?= (426880 % 13591409)^2 * 10005

-- pi tests
piDouble :: Assertion
piDouble = getValue (Exact 1 1) @?= (pi :: Double)

piMatchesOeis :: Assertion
piMatchesOeis = getValue (Exact 1 1) @?= oeisValue

-- https://oeis.org/A000796
oeisValue :: Fixed (E 104)
oeisValue = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214

-- digits 762 to 767 of pi are 999999
feynmanPoint :: Assertion
feynmanPoint = x `mod` 1000000 @?= 999999
  where
    MkFixed x = getValue (Exact 1 1) :: Fixed (E 767)

-- last 21 digits of pi on wolfram alpha (http://www.wolframalpha.com/input/?i=pi)
-- by asking for more digits as much as possible
piWAend :: Assertion
piWAend = x `mod` (10^21) @?= 706420467525907091548
  where
    MkFixed x = getValue (Exact 1 1) :: Fixed (E 3647)

-- pi power tests
-- http://www.wolframalpha.com/input/?i=1000th+digit+of+pi%5E3%2F10
pi3 :: Assertion
pi3 = x `mod` 100 @?= 98
  where
    MkFixed x = getValue (Exact 3 (1 % 10)) :: Fixed (E 1000)

-- http://www.wolframalpha.com/input/?i=1000th+digit+of+pi%5E-1+*+10
piNegOne :: Assertion
piNegOne = x `mod` 100 @?= 87
  where
    MkFixed x = getValue (Exact (-1) 10) :: Fixed (E 1000)

-- http://www.wolframalpha.com/input/?i=1000th+digit+of+pi%5E10+%2F+10%5E4
pi10 :: Assertion
pi10 = x `mod` 100 @?= 58
  where
    MkFixed x = getValue (Exact 10 (1 % 10^4)) :: Fixed (E 1000)

-- http://www.wolframalpha.com/input/?i=1000th+digit+of+pi%5E-10+*+100000
piNeg10 :: Assertion
piNeg10 = x `mod` 100 @?= 01
  where
    MkFixed x = getValue (Exact (-10) (10^5)) :: Fixed (E 1000)

-- http://www.wolframalpha.com/input/?i=400th+digit+of+pi%5E51+*+10%5E-25
pi51 :: Assertion
pi51 = x `mod` 100 @?= 39
  where
    MkFixed x = getValue (Exact 51 (1 % 10^25)) :: Fixed (E 400)

-- http://www.wolframalpha.com/input/?i=400th+digit+of+pi%5E-51+*+10%5E26
piNeg51 :: Assertion
piNeg51 = x `mod` 100 @?= 93
  where
    MkFixed x = getValue (Exact (-51) (10^26)) :: Fixed (E 400)

-- exact value of riemann zeta(50): should be very near 1
zeta50 :: ExactPi
zeta50 = Exact 50 (39604576419286371856998202 % 285258771457546764463363635252374414183254365234375)

zeta200 :: ExactPi
zeta200 = Exact 200 (996768098856666829529857264280799324216991774914413349936111645234527339243047375137731604604421998265202825395226558782117309054290681031680198580956052700765605768743424718675968548245722319600560038220395777111787342302 % 2682678748792657844957504192313280657551803049278355275671666881580642758576467817615493645217977237214155689404787155170845497733836863647685885197919191727452679238952541411298115541287013688972773507748859386210346035176197388875022427877722880764252312145081723341902733317236524547144682628641021437942981719970703125)

-- value of zeta(50) - 1 from wolfram alpha (up to a Double)
-- http://www.wolframalpha.com/input/?i=zeta(50)-1
zeta50MinusOne :: Assertion
zeta50MinusOne = t @?= 8.8817842109308159e-16
  where
    t = getRationalLimit (==) . map (subtract 1) . rationalApproximations $ zeta50 :: Double

-- http://www.wolframalpha.com/input/?i=zeta(200)-1
zeta200MinusOne :: Assertion
zeta200MinusOne = t @?= 6.2230152778611417071e-61
  where
    t = getRationalLimit (==) . map (subtract 1) . rationalApproximations $ zeta200 :: Double

-- test against bbp formula
prop :: Positive Integer -> Bool
prop (Positive n) = getDigit n == getDigitBBP (n - 1)

tests :: TestTree
tests = testGroup "Rational approximation tests"
  [ testGroup "π² tests" [ testCase "matches double precision"       piSquaredDouble
                         , testCase "matches start of wolfram alpha" piSquaredWAstart
                         , testCase "matches end of wolfram alpha"   piSquaredWAend
                         , testCase "first term matches chudnovsky"  firstApproximation
                         ]
  , testGroup "π tests"  [ testCase "matches double precision"       piDouble
                         , testCase "matches oeis digits"            piMatchesOeis
                         , testCase "has feynman point"              feynmanPoint
                         , testCase "matches end of wolfram alpha"   piWAend
                         ]
  , testGroup "πᵏ tests" [ testCase "digits near 1000 of k=3"        pi3
                         , testCase "digits near 1000 of k=-1"       piNegOne
                         , testCase "digits near 1000 of k=10"       pi10
                         , testCase "digits near 1000 of k=-10"      piNeg10
                         , testCase "digits near 400 of k=51"        pi51
                         , testCase "digits near 400 of k=-51"       piNeg51
                         , testCase "ζ(50)-1 double precision"       zeta50MinusOne
                         , testCase "ζ(500)-1 double precision"      zeta200MinusOne
                         ]
  , testProperty "hex digits match BBP formula" prop
  ]

main :: IO ()
main = defaultMain tests
