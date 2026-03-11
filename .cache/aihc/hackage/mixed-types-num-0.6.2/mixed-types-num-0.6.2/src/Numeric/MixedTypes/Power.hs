{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Power
    Description :  Bottom-up typed exponentiation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Power
(
  -- * Exponentiation
   CanPow(..), CanPowBy
  , (^), (^^)
  , powUsingMul, integerPowCN
  , powUsingMulRecip
  , CanTestIsIntegerType(..)
  -- ** Tests
  , specCanPow
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import Numeric.CollectErrors ( CN, cn, unCN )
import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Mul
-- import Numeric.MixedTypes.Div ()



{---- Exponentiation -----}

infixl 8  ^, ^^

(^) :: (CanPow t1 t2) => t1 -> t2 -> PowType t1 t2
(^) = pow

(^^) :: (CanPow t1 t2) => t1 -> t2 -> PPowType t1 t2
(^^) = ppow


{-|
  A replacement for Prelude's binary `P.^` and `P.^^`.
-}
class CanPow b e where
  type PowType b e
  type PPowType b e
  type PPowType b e = PowType b e
  type PowType b e = b -- default
  pow :: b -> e -> PowType b e
  ppow :: b -> e -> PPowType b e
  default ppow :: (PPowType b e ~ PowType b e) => b -> e -> PPowType b e
  ppow = pow

{-|
  Ability to detect whether a numeric type is restricted to (a subset of) integers.
  
  This is useful eg when checking the arguments of the power operator in the CN instance for power.
-}
class CanTestIsIntegerType t where
  isIntegerType :: t -> Bool
  isIntegerType _ = False

instance CanTestIsIntegerType t => CanTestIsIntegerType (CN t) where
  isIntegerType t = isIntegerType (unCN t)

instance CanTestIsIntegerType Int where
  isIntegerType _ = True

instance CanTestIsIntegerType Integer where
  isIntegerType _ = True

instance CanTestIsIntegerType Rational
instance CanTestIsIntegerType Double

integerPowCN ::
  (HasOrderCertainly b Integer, HasOrderCertainly e Integer,
   HasEqCertainly b Integer, HasEqCertainly e Integer)
  =>
  (b -> e -> r) -> CN b -> CN e -> CN r
integerPowCN unsafeIntegerPow b n
  | n !<! 0 =
    CN.noValueNumErrorCertain $ CN.OutOfDomain "illegal integer pow: negative exponent"
  | n !==! 0 && b !==! 0 =
    CN.noValueNumErrorCertain $ CN.OutOfDomain "illegal integer pow: 0^0"
  | n ?<? 0 =
    CN.noValueNumErrorCertain $ CN.OutOfDomain "illegal integer pow: negative exponent"
  | n ?==? 0 && b ?==? 0 =
    CN.noValueNumErrorPotential $ CN.OutOfDomain "illegal integer pow: 0^0"
  | otherwise =
    CN.lift2 unsafeIntegerPow b n

powCN ::
  (HasOrderCertainly b Integer, HasOrderCertainly e Integer,
   HasEqCertainly b Integer, CanTestIsIntegerType b, CanTestIsIntegerType e, CanTestInteger e)
  =>
  (b -> e -> r) -> CN b -> CN e -> CN r
powCN unsafePow b e
  | isIntegerType b && isIntegerType e && e !<! 0 =
    CN.noValueNumErrorCertain $ CN.OutOfDomain "illegal integer pow: negative exponent, consider using ppow or (^^)"
  | otherwise  = ppowCN unsafePow b e

ppowCN ::
  (HasOrderCertainly b Integer, HasOrderCertainly e Integer,
   HasEqCertainly b Integer, CanTestInteger e)
  =>
  (b -> e -> r) -> CN b -> CN e -> CN r
ppowCN unsafePow b e
  | b !==! 0 && e !<=! 0 =
    CN.noValueNumErrorCertain $ CN.OutOfDomain "illegal pow: 0^e with e <= 0"
  | b !<! 0 && certainlyNotInteger e =
    CN.noValueNumErrorCertain $ CN.OutOfDomain "illegal pow: b^e with b < 0 and e non-integer"
  | b ?==? 0 && e ?<=? 0 =
    CN.noValueNumErrorPotential $ CN.OutOfDomain "illegal pow: 0^e with e <= 0"
  | b ?<? 0 && not (certainlyInteger e) =
    CN.noValueNumErrorPotential $ CN.OutOfDomain "illegal pow: b^e with b < 0 and e non-integer"
  | otherwise =
    CN.lift2 unsafePow b e

powUsingMul ::
  (CanBeInteger e)
   =>
   t -> (t -> t -> t) -> t -> e -> t
powUsingMul one mul' x nPre
  | n < 0 = error $ "powUsingMul is not defined for negative exponent " ++ show n
  | n == 0 = one
  | otherwise = aux n
  where
    (.*) = mul'
    n = integer nPre
    aux m
      | m == 1 = x
      | even m =
        let s = aux (m `P.div` 2) in s .* s
      | otherwise =
        let s = aux ((m-1) `P.div` 2) in x .* s .* s

powUsingMulRecip ::
  (CanBeInteger e)
   =>
   t -> (t -> t -> t) -> (t -> t) -> t -> e -> t
powUsingMulRecip one mul' recip' x e
  | eI < 0 = recip' $ powUsingMul one mul' x (negate eI)
  | otherwise = powUsingMul one mul' x eI
  where
  eI = integer e

type CanPowBy t1 t2 =
  (CanPow t1 t2, PowType t1 t2 ~ t1)

{-|
  HSpec properties that each implementation of CanPow should satisfy.
 -}
specCanPow ::
  _ => T t1 -> T t2 -> Spec
specCanPow (T typeName1 :: T t1) (T typeName2 :: T t2) =
  describe (printf "CanPow %s %s" typeName1 typeName2) $ do
    it "x^0 = 1" $ do
      property $ \ (x :: t1) (sampleT1 :: t1) (sampleT2 :: t2) ->
        let one = (convertExactlyWithSample sampleT1 1 :: t1) in
        let z = (convertExactlyWithSample sampleT2 0 :: t2) in
        (x ^ z) ?==?$ one
    it "x^1 = x" $ do
      property $ \ (x :: t1) (sampleT2 :: t2) ->
        let one = (convertExactlyWithSample sampleT2 1 :: t2) in
        (x ^ one) ?==?$ x
    it "x^(y+1) = x*x^y" $ do
      property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyNonNegative y) ==>
          x * (x ^ y) ?==?$ (x ^ (y + 1))
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

instance CanPow Integer Integer where  
  type PowType Integer Integer = Integer
  type PPowType Integer Integer = Rational
  pow b = (P.^) b
  ppow b = (P.^^) (rational b)
instance CanPow Integer Int where
  type PowType Integer Int = Integer
  type PPowType Integer Int = Rational
  pow b = (P.^) b
  ppow b = (P.^^) (rational b)
instance CanPow Int Integer where
  type PowType Int Integer = Integer
  type PPowType Int Integer = Rational
  pow b = (P.^) (integer b)
  ppow b = (P.^^) (rational b)
instance CanPow Int Int where
  type PowType Int Int = Rational
  pow b = (P.^^) (rational b)
instance CanPow Rational Int where
  pow = (P.^^)
instance CanPow Rational Integer where
  pow = (P.^^)
instance CanPow Double Int where
  pow = (P.^^)
instance CanPow Double Integer where
  pow = (P.^^)
instance CanPow Double Double where
  type PowType Double Double = Double
  pow = (P.**)
instance CanPow Double Rational where
  type PowType Double Rational = Double
  pow b e = b ^ (double e)
instance CanPow Rational Double where
  type PowType Rational Double = Double
  pow b e = (double b) ^ e
instance CanPow Integer Double where
  type PowType Integer Double = Double
  pow b e = (double b) ^ e
instance CanPow Int Double where
  type PowType Int Double = Double
  pow b e = (double b) ^ e

-- instance (CanPow a b) => CanPow [a] [b] where
--   type PowType [a] [b] = [PowType a b]
--   pow (x:xs) (y:ys) = (pow x y) : (pow xs ys)
--   pow _ _ = []

instance (CanPow a b) => CanPow (Maybe a) (Maybe b) where
  type PowType (Maybe a) (Maybe b) = Maybe (PowType a b)
  pow (Just x) (Just y) = Just (pow x y)
  pow _ _ = Nothing

instance
  (CanPow b e, HasOrderCertainly b Integer, HasOrderCertainly e Integer,
   HasEqCertainly b Integer, CanTestIsIntegerType b, CanTestIsIntegerType e, CanTestInteger e)
  =>
  CanPow (CN b) (CN e)
  where
  type PowType (CN b) (CN e) = CN (PowType b e)
  type PPowType (CN b) (CN e) = CN (PPowType b e)
  pow = powCN pow
  ppow = ppowCN ppow

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanPow $t e, HasOrderCertainly e Integer, CanTestIsIntegerType e, CanTestInteger e)
      =>
      CanPow $t (CN e)
      where
      type PowType $t (CN e) = CN (PowType $t e)
      pow b e = powCN pow (cn b) e
      type PPowType $t (CN e) = CN (PPowType $t e)
      ppow b e = ppowCN ppow (cn b) e

    instance
      (CanPow b $t, HasOrderCertainly b Integer, HasEqCertainly b Integer, CanTestIsIntegerType b)
      =>
      CanPow (CN b) $t
      where
      type PowType (CN b) $t = CN (PowType b $t)
      pow b e = powCN pow b (cn e)
      type PPowType (CN b) $t = CN (PPowType b $t)
      ppow b e = ppowCN ppow b (cn e)

  |]))
