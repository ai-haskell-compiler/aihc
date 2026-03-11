{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Round
    Description :  Bottom-up typed round, floor, etc.
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Round
(
  -- * Rounded division + modulus
  CanDivIMod(..)
  , CanDivIModIntegerSameType
  , CanDivIModIntegerSameTypeCN
  -- * Rounding
  , CanRound(..), HasIntegerBounds(..)
  -- ** Tests
  , specCanDivIMod, specCanRound, specHasIntegerBounds
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf
import Data.Fixed (divMod')

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck as QC

import Numeric.CollectErrors ( CN )
import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Mul

{----  rounded division + modulo -----}

class CanDivIMod t1 t2 where
  type DivIType t1 t2
  type ModType t1 t2
  type ModType t1 t2 = t1
  divIMod :: t1 -> t2 -> (DivIType t1 t2, ModType t1 t2)
  mod :: t1 -> t2 -> ModType t1 t2
  mod a b = snd $ divIMod a b
  divI :: t1 -> t2 -> DivIType t1 t2
  divI a b = fst $ divIMod a b

type CanDivIModIntegerSameType t =
  (CanDivIMod t t, DivIType t t ~ Integer, ModType t t ~ t)

type CanDivIModIntegerSameTypeCN t =
  (CanDivIMod t t, DivIType t t ~ CN Integer, ModType t t ~ t)

instance CanDivIMod Integer Integer where
  type DivIType Integer Integer = Integer
  divIMod = P.divMod

instance CanDivIMod Integer Int where
  type DivIType Integer Int = Integer
  divIMod x m = divIMod x (integer m)

instance CanDivIMod Int Integer where
  type ModType Int Integer = Integer
  type DivIType Int Integer = Integer
  divIMod x m = divIMod (integer x) m

instance CanDivIMod Int Int where
  type ModType Int Int = Integer
  type DivIType Int Int = Integer
  divIMod x m = divIMod (integer x) (integer m)

instance (CanDivIMod t1 t2, CanTestPosNeg t2) => CanDivIMod (CN t1) (CN t2) where
  type DivIType (CN t1) (CN t2) = (CN (DivIType t1 t2))
  type ModType (CN t1) (CN t2) = (CN (ModType t1 t2))
  divIMod x m
    | isCertainlyPositive m = (d, xm)
    | isCertainlyNegative m = (noval d, noval xm)
    | otherwise = (errPote d, errPote xm)
    where
    (d,xm) = CN.lift2pair divIMod x m

noval :: CN v -> CN v
noval = flip CN.removeValueErrorCertain err
errPote :: CN t -> CN t
errPote = CN.prependErrorPotential err
err :: CN.NumError
err = CN.OutOfDomain "divIMod: modulus not positive"

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance (CanDivIMod t1 $t) => CanDivIMod (CN t1) $t where
      type DivIType (CN t1) $t = (CN (DivIType t1 $t))
      type ModType (CN t1) $t = (CN (ModType t1 $t))
      divIMod x m
        | isCertainlyPositive m = (d, xm)
        | isCertainlyNegative m = (noval d, noval xm)
        | otherwise = (errPote d, errPote xm)
        where
        (d,xm) = CN.lift1Tpair divIMod x m

    instance (CanDivIMod $t t2, CanTestPosNeg t2) => CanDivIMod $t (CN t2) where
      type DivIType $t (CN t2) = (CN (DivIType $t t2))
      type ModType $t (CN t2) = (CN (ModType $t t2))
      divIMod x m
        | isCertainlyPositive m = (d, xm)
        | isCertainlyNegative m = (noval d, noval xm)
        | otherwise = (errPote d, errPote xm)
        where
        (d,xm) = CN.liftT1pair divIMod x m
  |]))

instance CanDivIMod Rational Rational where
  type DivIType Rational Rational = Integer
  divIMod = divMod'

instance CanDivIMod Rational Integer where
  type DivIType Rational Integer = Integer
  divIMod x m = divIMod x (rational m)

instance CanDivIMod Rational Int where
  type DivIType Rational Int = Integer
  divIMod x m = divIMod x (rational m)

instance CanDivIMod Integer Rational where
  type ModType Integer Rational = Rational
  type DivIType Integer Rational = Integer
  divIMod x m = divIMod (rational x) m

instance CanDivIMod Int Rational where
  type ModType Int Rational = Rational
  type DivIType Int Rational = Integer
  divIMod x m = divIMod (rational x) m

instance CanDivIMod Double Double where
  type DivIType Double Double = Integer
  divIMod = divMod'

instance CanDivIMod Double Integer where
  type DivIType Double Integer = Integer
  divIMod x m = divIMod x (double m)

{-|
  HSpec properties that each implementation of CanRound should satisfy.
 -}
specCanDivIMod ::
  _ => T t -> Spec
specCanDivIMod (T typeName :: T t) =
  describe (printf "CanDivMod %s %s" typeName typeName) $ do
    it "holds 0 <= x `mod` m < m" $ do
      property $ \ (x :: t)  (m :: t) ->
        isFinite x && m !>! 0 ==>
          let xm = x `mod` m in
          (0 ?<=?$ xm) .&&. (xm ?<?$ m)
    it "holds x == (x `div'` m)*m + (x `mod` m)" $ do
      property $ \ (x :: t)  (m :: t) ->
        isFinite x && m !>! 0 ==>
          let (d,xm) = divIMod x m in
          (x ?==?$ (d*m + xm))
  where
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)
  (?<?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<?$) = printArgsIfFails2 "?<?" (?<?)
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

{----  rounding -----}

{-|
  A replacement for Prelude's `P.RealFrac` operations, such as round in
  which the result type is fixed to Integer.

  If @RealFrac t@ and @CanTestPosNeg t@,
  then one can use the default implementation to mirror Prelude's @round@, etc.

  In other cases, it is sufficient to define `properFraction`.
-}
class CanRound t where
  type RoundType t
  type RoundType t = Integer
  properFraction :: t -> (RoundType t, t)
  default properFraction :: (P.RealFrac t, RoundType t ~ Integer) => t -> (RoundType t, t)
  properFraction = P.properFraction
  truncate :: t -> RoundType t
  truncate = fst . properFraction
  round :: t -> RoundType t
  default round :: (HasOrderCertainly t Rational, RoundType t ~ Integer) => t -> RoundType t
  round x
    | -0.5 !<! r && r !<! 0.5 = n
    | r !<! -0.5 = n - 1
    | r !>! 0.5 = n + 1
    | even n = n
    | r !<! 0.0 = n - 1
    | r !>! 0.0 = n + 1
    | otherwise = error "round default defn: Bad value"
    where
    (n,r) = properFraction x
  ceiling :: t -> RoundType t
  default ceiling :: (CanTestPosNeg t, RoundType t ~ Integer) => t -> RoundType t
  ceiling x
    | isCertainlyPositive r = n + 1
    | otherwise = n
    where
    (n,r) = properFraction x
  floor :: t -> RoundType t
  default floor :: (CanTestPosNeg t, RoundType t ~ Integer) => t -> RoundType t
  floor x
    | isCertainlyNegative r = n - 1
    | otherwise = n
    where
    (n,r) = properFraction x

instance CanRound Rational
instance CanRound Double where
  round = P.round
  ceiling = P.ceiling
  floor = P.floor

{-|
  HSpec properties that each implementation of CanRound should satisfy.
 -}
specCanRound ::
  _ => T t -> Spec
specCanRound (T typeName :: T t) =
  describe (printf "CanRound %s" typeName) $ do
    it "holds floor x <= x <= ceiling x" $ do
      property $ \ (x :: t) ->
        isFinite x ==>
          (floor x ?<=?$ x) .&&. (x ?<=?$ ceiling x)
    it "holds floor x <= round x <= ceiling x" $ do
      property $ \ (x :: t) ->
        isFinite x ==>
          (floor x !<=!$ round x) .&&. (round x !<=!$ ceiling x)
    it "0 <= ceiling x - floor x <= 1" $ do
      property $ \ (x :: t) ->
        isFinite x ==>
          let diffCeilingFloorX = ceiling x - floor x in
          (0 ?<=? diffCeilingFloorX) .&&. (diffCeilingFloorX ?<=? 1)
    it "holds floor x = round x = ceiling x for integers" $ do
      property $ \ (xi :: Integer) (sampleT :: t) ->
        let x = convertExactlyWithSample sampleT xi :: t in
          (floor x !==!$ round x) .&&. (round x !==!$ ceiling x)
  where
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)
  (!<=!$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (!<=!$) = printArgsIfFails2 "!<=!" (!<=!)
  (!==!$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (!==!$) = printArgsIfFails2 "!==!" (!==!)


class HasIntegerBounds t where
  integerBounds :: t -> (Integer, Integer)
  default integerBounds :: (CanRound t, RoundType t ~ Integer) => t -> (Integer, Integer)
  integerBounds x = (floor x, ceiling x)

instance HasIntegerBounds Rational
instance HasIntegerBounds Double
instance HasIntegerBounds Integer where
  integerBounds n = (n,n)
instance HasIntegerBounds Int where
  integerBounds n = (n',n') where n' = integer n

{-|
  HSpec properties that each implementation of CanRound should satisfy.
 -}
specHasIntegerBounds ::
  _ => T t -> Spec
specHasIntegerBounds (T typeName :: T t) =
  describe (printf "HasIntegerBounds %s" typeName) $ do
    it "holds l <= x <= r" $ do
      property $ \ (x :: t) ->
        isFinite x ==>
          let (l,r) = integerBounds x in
          (l ?<=?$ x) .&&. (x ?<=?$ r)
  where
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)
