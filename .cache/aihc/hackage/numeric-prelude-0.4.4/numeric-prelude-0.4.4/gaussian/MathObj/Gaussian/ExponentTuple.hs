{-# LANGUAGE RebindableSyntax #-}
module MathObj.Gaussian.ExponentTuple where

import qualified Test.QuickCheck as QC

import Control.Applicative (liftA2, liftA3)

import Data.Function.HT (compose2)

import NumericPrelude.Base as P
import NumericPrelude.Numeric as NP


{- $setup
>>> import MathObj.Gaussian.ExponentTuple (HoelderConjugates(HoelderConjugates))
>>> import MathObj.Gaussian.ExponentTuple (YoungConjugates(YoungConjugates))
>>> import NumericPrelude.Base as P
>>> import NumericPrelude.Numeric as NP
>>> import Prelude ()
-}


{- |
For @(HoelderConjugates p q)@ it holds

prop> \(HoelderConjugates p q)  ->  p>=1 && q>=1 && 1/p + 1/q == 1
-}
data HoelderConjugates = HoelderConjugates Rational Rational
   deriving Show

instance QC.Arbitrary HoelderConjugates where
   arbitrary = genHoelderConjugates0

genHoelderConjugates0 :: QC.Gen HoelderConjugates
genHoelderConjugates0 =
   liftA2
      (\(QC.Positive p) (QC.Positive q) ->
         let s = p + q in HoelderConjugates (s % p) (s % q))
      QC.arbitrary QC.arbitrary

genHoelderConjugates1 :: QC.Gen HoelderConjugates
genHoelderConjugates1 =
   liftA2
      (\(QC.Positive p) (QC.Positive q) ->
         let s = 1%p + 1%q
         in HoelderConjugates (fromInteger p * s) (fromInteger q * s))
      QC.arbitrary QC.arbitrary


{- |
For @(YoungConjugates p q r)@ it holds

prop> \(YoungConjugates p q r)  ->  p>=1 && q>=1 && r>=1 && 1/p + 1/q == 1/r + 1
-}
data YoungConjugates = YoungConjugates Rational Rational Rational
   deriving Show

instance QC.Arbitrary YoungConjugates where
   arbitrary = genYoungConjugates0

{-
Find positive natural numbers @a, b, c, d@ with

> a + b = c + d

and

> d >= a, d >= b, d >= c

then set

> p=d/a, q=d/b, r=d/c


a+b<=c
b+c<=a
->  2b <= 0
-}
genYoungConjugates0 :: QC.Gen YoungConjugates
genYoungConjugates0 =
   liftA3
      (\(QC.Positive a0) (QC.Positive b0) (QC.Positive c0) ->
         let guardSwap cond (x,y) =
                if cond x y then (x,y) else (y,x)
             {-
             If a+b<=c, then from b>0 it follows a<c and thus c+b>a.
             Swapping a and c is enough and we have not to consider more cases.
             -}
             (a1,c1) = guardSwap (\a c -> a+b0>c) (a0,c0)
             b1 = b0
             d1 = a1+b1-c1
             ((a2,b2),(c2,d2)) =
                guardSwap (compose2 (<=) snd)
                   (guardSwap (<=) (a1,b1),
                    guardSwap (<=) (c1,d1))
         in  YoungConjugates (d2%a2) (d2%b2) (d2%c2))
      QC.arbitrary QC.arbitrary QC.arbitrary

{- |
This one is simpler, but may yield exponents smaller than 1.
-}
genYoungConjugates1 :: QC.Gen YoungConjugates
genYoungConjugates1 =
   liftA3
      (\(QC.Positive a0) (QC.Positive b0) (QC.Positive c0) ->
         let {-
             If a+b<=c, then from b>0 it follows a<c and thus c+b>a.
             Swapping a and c is enough and we have not to consider more cases.
             -}
             (a1,c1) = if a0+b0<=c0 then (c0,a0) else (a0,c0)
             b1 = b0
             d1 = a1+b1-c1
         in  YoungConjugates (d1%a1) (d1%b1) (d1%c1))
      QC.arbitrary QC.arbitrary QC.arbitrary
