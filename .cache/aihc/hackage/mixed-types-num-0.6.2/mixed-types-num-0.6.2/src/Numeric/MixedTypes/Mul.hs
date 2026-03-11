{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Mul
    Description :  Bottom-up typed multiplication with ring laws
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Mul
(
  -- ** Multiplication
  CanMul, CanMulAsymmetric(..), CanMulBy, CanMulSameType
  , (*), product, productWithSample
  -- ** Tests
  , specCanMul, specCanMulNotMixed, specCanMulSameType
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import qualified Numeric.CollectErrors as CN
import Numeric.CollectErrors ( CN )

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Reduce

{---- Multiplication -----}

type CanMul t1 t2 =
  (CanMulAsymmetric t1 t2, CanMulAsymmetric t2 t1,
   MulType t1 t2 ~ MulType t2 t1)

{-|
  A replacement for Prelude's `P.*`.  If @t1 = t2@ and @Num t1@,
  then one can use the default implementation to mirror Prelude's @*@.
-}
class CanMulAsymmetric t1 t2 where
  type MulType t1 t2
  type MulType t1 t2 = t1 -- default
  mul :: t1 -> t2 -> MulType t1 t2
  default mul :: (MulType t1 t2 ~ t1, t1~t2, P.Num t1) => t1 -> t2 -> MulType t1 t2
  mul = (P.*)

infixl 7  *

(*) :: (CanMulAsymmetric t1 t2) => t1 -> t2 -> MulType t1 t2
(*) = mul

type CanMulBy t1 t2 =
  (CanMul t1 t2, MulType t1 t2 ~ t1)
type CanMulSameType t =
  CanMulBy t t

product :: (CanMulSameType t, ConvertibleExactly Integer t) => [t] -> t
product xs = List.foldl' mul (convertExactly 1) xs

productWithSample :: (CanMulSameType t, ConvertibleExactlyWithSample Integer t) => t -> [t] -> t
productWithSample sampleT xs = List.foldl' mul (convertExactlyWithSample sampleT 1) xs

{-|
  HSpec properties that each implementation of CanMul should satisfy.
 -}
specCanMul ::
  _ => T t1 -> T t2 -> T t3 -> Spec
specCanMul (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanMul %s %s, CanMul %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "absorbs 1" $ do
      property $ \ (x :: t1) (sampleT2 :: t2) -> 
        let one = (convertExactlyWithSample sampleT2 1 :: t2) in (x * one) ?==?$ x
    it "is commutative" $ do
      property $ \ (x :: t1) (y :: t2) -> (x * y) ?==?$ (y * x)
    it "is associative" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x * (y * z)) ?==?$ ((x * y) * z)
    it "distributes over addition" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x * (y + z)) ?==?$ (x * y) + (x * z)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

{-|
  HSpec properties that each implementation of CanMul should satisfy.
 -}
specCanMulNotMixed ::
  _ => T t -> Spec
specCanMulNotMixed (t :: T t) = specCanMul t t t

{-|
  HSpec properties that each implementation of CanMulSameType should satisfy.
 -}
specCanMulSameType ::
  (Show t, Arbitrary t, ConvertibleExactlyWithSample Integer t,
   CanTestCertainly (EqCompareType t t), HasEqAsymmetric t t,
   CanMulAsymmetric t t, MulType t t ~ t)
   =>
   T t -> Spec
specCanMulSameType (T typeName :: T t) =
  describe (printf "CanMulSameType %s" typeName) $ do
    it "has product working over integers" $ do
      property $ \ (xsi :: [Integer]) (sampleT :: t) ->
        (productWithSample sampleT $ (map (convertExactlyWithSample sampleT) xsi :: [t])) 
        ?==?$ 
        (convertExactlyWithSample sampleT (product xsi) :: t)
    it "has product [] = 1" $ do
      property $ \ (sampleT :: t) ->
        (productWithSample sampleT ([] :: [t])) ?==?$ (convertExactlyWithSample sampleT 1 :: t)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

instance CanMulAsymmetric Int Int where
  type MulType Int Int = Integer -- do not risk overflow
  mul a b = (integer a) P.* (integer b)
instance CanMulAsymmetric Integer Integer
instance CanMulAsymmetric Rational Rational
instance CanMulAsymmetric Double Double

instance CanMulAsymmetric Int Integer where
  type MulType Int Integer = Integer
  mul = convertFirst mul
instance CanMulAsymmetric Integer Int where
  type MulType Integer Int = Integer
  mul = convertSecond mul

instance CanMulAsymmetric Int Rational where
  type MulType Int Rational = Rational
  mul = convertFirst mul
instance CanMulAsymmetric Rational Int where
  type MulType Rational Int = Rational
  mul = convertSecond mul

instance CanMulAsymmetric Integer Rational where
  type MulType Integer Rational = Rational
  mul = convertFirst mul
instance CanMulAsymmetric Rational Integer where
  type MulType Rational Integer = Rational
  mul = convertSecond mul

instance CanMulAsymmetric Int Double where
  type MulType Int Double = Double
  mul n d = mul (double n) d
instance CanMulAsymmetric Double Int where
  type MulType Double Int = Double
  mul d n = mul d (double n)

instance CanMulAsymmetric Integer Double where
  type MulType Integer Double = Double
  mul n d = mul (double n) d
instance CanMulAsymmetric Double Integer where
  type MulType Double Integer = Double
  mul d n = mul d (double n)

instance CanMulAsymmetric Rational Double where
  type MulType Rational Double = Double
  mul n d = mul (double n) d
instance CanMulAsymmetric Double Rational where
  type MulType Double Rational = Double
  mul d n = mul d (double n)

instance (CanMulAsymmetric a b) => CanMulAsymmetric [a] [b] where
  type MulType [a] [b] = [MulType a b]
  mul (x:xs) (y:ys) = (mul x y) : (mul xs ys)
  mul _ _ = []

instance (CanMulAsymmetric a b) => CanMulAsymmetric (Maybe a) (Maybe b) where
  type MulType (Maybe a) (Maybe b) = Maybe (MulType a b)
  mul (Just x) (Just y) = Just (mul x y)
  mul _ _ = Nothing

instance
  (CanMulAsymmetric a b, CanGiveUpIfVeryInaccurate (MulType a b))
  =>
  CanMulAsymmetric (CN a) (CN b)
  where
  type MulType (CN a) (CN b) = CN (MulType a b)
  mul a b = giveUpIfVeryInaccurate $ CN.lift2 mul a b

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanMulAsymmetric $t b, CanGiveUpIfVeryInaccurate (MulType $t b))
      =>
      CanMulAsymmetric $t (CN b)
      where
      type MulType $t (CN b) = CN (MulType $t b)
      mul a b = giveUpIfVeryInaccurate $ CN.liftT1 mul a b

    instance
      (CanMulAsymmetric a $t, CanGiveUpIfVeryInaccurate (MulType a $t))
      =>
      CanMulAsymmetric (CN a) $t
      where
      type MulType (CN a) $t = CN (MulType a $t)
      mul a b = giveUpIfVeryInaccurate $ CN.lift1T mul a b
  |]))
