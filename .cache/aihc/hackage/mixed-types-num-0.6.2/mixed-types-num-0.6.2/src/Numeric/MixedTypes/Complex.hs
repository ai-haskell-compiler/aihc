{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Complex
    Description :  Instances for Data.Complex
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Instances for "Data.Complex".
-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.MixedTypes.Complex
(
  tComplex
)
where

import Numeric.MixedTypes.PreludeHiding
-- import qualified Prelude as P
-- import Text.Printf

import Utils.TH.DeclForTypes

import Data.Complex

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Mul
import Numeric.MixedTypes.Div
-- import Numeric.MixedTypes.Power
-- import Numeric.MixedTypes.Field
import Numeric.MixedTypes.Elementary

tComplex :: T t -> T (Complex t)
tComplex (T tName) = T ("(Complex " ++ tName ++ ")")

instance (ConvertibleExactly Integer t) => (ConvertibleExactly Integer (Complex t))
  where
  safeConvertExactly n =
    do
    nT <- safeConvertExactly n
    zT <- safeConvertExactly 0
    return $ nT :+ zT

instance (ConvertibleExactly Int t) => (ConvertibleExactly Int (Complex t))
  where
  safeConvertExactly n =
    do
    nT <- safeConvertExactly n
    zT <- safeConvertExactly (int 0)
    return $ nT :+ zT

instance (ConvertibleExactly Rational t) => (ConvertibleExactly Rational (Complex t))
  where
  safeConvertExactly r =
    do
    rT <- safeConvertExactly r
    zT <- safeConvertExactly 0.0
    return $ rT :+ zT

instance (ConvertibleExactly t1 t2) => (ConvertibleExactly (Complex t1) (Complex t2))
  where
  safeConvertExactly (a1 :+ i1) =
    do
    a2 <- safeConvertExactly a1
    i2 <- safeConvertExactly i1
    return $ a2 :+ i2

instance (HasEqAsymmetric a b) => HasEqAsymmetric (Complex a) (Complex b) where
  type EqCompareType (Complex a) (Complex b) = EqCompareType a b
  equalTo (a1 :+ i1) (a2 :+ i2) = (a1 == a2) && (i1 == i2)

instance (CanTestInteger t, CanTestZero t) => CanTestInteger (Complex t) where
  certainlyNotInteger (a :+ i) =
    certainlyNotInteger a || isCertainlyNonZero i
  certainlyIntegerGetIt (a :+ i) =
    case (certainlyIntegerGetIt a, certainlyIntegerGetIt i) of
      (Just aN, Just iN) | iN == 0 -> Just aN
      _ -> Nothing

instance CanNeg t => CanNeg (Complex t) where
  type NegType (Complex t) = Complex (NegType t)
  negate (a :+ i) = (negate a) :+ (negate i)

instance (CanAddAsymmetric a b) => CanAddAsymmetric (Complex a) (Complex b) where
  type AddType (Complex a) (Complex b) = Complex (AddType a b)
  add (a1 :+ i1) (a2 :+ i2) = (a1 + a2) :+ (i1 + i2)

instance (CanSub a b) => CanSub (Complex a) (Complex b) where
  type SubType (Complex a) (Complex b) = Complex (SubType a b)
  sub (a1 :+ i1) (a2 :+ i2) = (a1 - a2) :+ (i1 - i2)

instance
  (CanMulAsymmetric a b
  , CanAddSameType (MulType a b), CanSubSameType (MulType a b))
  =>
  CanMulAsymmetric (Complex a) (Complex b)
  where
  type MulType (Complex a) (Complex b) = Complex (MulType a b)
  mul (a1 :+ i1) (a2 :+ i2) =
    (a1*a2 - i1*i2) :+ (a1*i2 + i1*a2)

instance
  (CanMulAsymmetric a b
  , CanAddSameType (MulType a b), CanSubSameType (MulType a b)
  , CanMulAsymmetric b b, CanAddSameType (MulType b b)
  , CanDiv (MulType a b) (MulType b b))
  =>
  CanDiv (Complex a) (Complex b)
  where
  type DivType (Complex a) (Complex b) = Complex (DivType (MulType a b) (MulType b b))
  divide (a1 :+ i1) (a2 :+ i2) =
    let d = a2*a2 + i2*i2 in
    ((a1*a2 + i1*i2)/d) :+ ((i1*a2-a1*i2)/d)

instance
  (CanMulAsymmetric t t
  , CanAddSameType (MulType t t)
  , CanSqrt (MulType t t))
  =>
  CanAbs (Complex t)
  where
  type AbsType (Complex t) = SqrtType (MulType t t)
  abs (a :+ i) = sqrt (a*a + i*i)

instance
  (CanExp t
  , CanSinCos t
  , CanMulAsymmetric (ExpType t) (SinCosType t))
  =>
  CanExp (Complex t)
  where
  type ExpType (Complex t) = Complex (MulType (ExpType t) (SinCosType t))
  exp (a :+ i) =
    let ea = exp a in
    (ea * cos i) :+ (ea * sin i)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance (HasEqAsymmetric $t b) => HasEqAsymmetric $t (Complex b) where
      type EqCompareType $t (Complex b) = EqCompareType $t b
      equalTo n (a2 :+ i2) = (n == a2) && (convertExactlyWithSample n 0 == i2)

    instance (HasEqAsymmetric a $t) => HasEqAsymmetric (Complex a) $t where
      type EqCompareType (Complex a) $t = EqCompareType a $t
      equalTo (a1 :+ i1) n = (a1 == n) && (i1 == convertExactlyWithSample n 0)

    instance (CanAddAsymmetric $t b) => CanAddAsymmetric $t (Complex b) where
      type AddType $t (Complex b) = Complex (AddType $t b)
      add n (a2 :+ i2) = (n + a2) :+ (convertExactlyWithSample n 0 + i2)

    instance (CanAddAsymmetric a $t) => CanAddAsymmetric (Complex a) $t where
      type AddType (Complex a) $t = Complex (AddType a $t)
      add (a1 :+ i1) n = (a1 + n) :+ (i1 + (convertExactlyWithSample n 0))

    instance (CanSub $t b) => CanSub $t (Complex b) where
      type SubType $t (Complex b) = Complex (SubType $t b)
      sub n (a2 :+ i2) = (n - a2) :+ (convertExactlyWithSample n 0 - i2)

    instance (CanSub a $t) => CanSub (Complex a) $t where
      type SubType (Complex a) $t = Complex (SubType a $t)
      sub (a1 :+ i1) n = (a1 - n) :+ (i1 - (convertExactlyWithSample n 0))

    instance
      (CanMulAsymmetric $t b) => CanMulAsymmetric $t (Complex b)
      where
      type MulType $t (Complex b) = Complex (MulType $t b)
      mul n (a2 :+ i2) = (n*a2) :+ (n*i2)

    instance
      (CanMulAsymmetric a $t) => CanMulAsymmetric (Complex a) $t
      where
      type MulType (Complex a) $t = Complex (MulType a $t)
      mul (a1 :+ i1) n = (a1*n) :+ (i1*n)

    instance
      (CanMulAsymmetric $t b
      , CanMulAsymmetric b b, CanAddSameType (MulType b b)
      , CanDiv (MulType $t b) (MulType b b))
      =>
      CanDiv $t (Complex b)
      where
      type DivType $t (Complex b) = Complex (DivType (MulType $t b) (MulType b b))
      divide n (a2 :+ i2) =
        let d = a2*a2 + i2*i2 in
        ((n*a2)/d) :+ (((-n)*i2)/d)

    instance
      (CanDiv a $t) => CanDiv (Complex a) $t
      where
      type DivType (Complex a) $t = Complex (DivType a $t)
      divide (a1 :+ i1) n = (a1/n) :+ (i1/n)
  |]))
