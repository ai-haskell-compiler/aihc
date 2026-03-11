{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-|
    Module      :  Numeric.MixedType.Elementary
    Description :  Bottom-up typed pi, sqrt, cos, etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}
module Numeric.MixedTypes.Elementary
(
  -- * Square root
  CanSqrt(..), CanSqrtSameType, specCanSqrtReal
  -- * Exp
  , CanExp(..), CanExpSameType, specCanExpReal
  -- * Log
  , CanLog(..), CanLogSameType, specCanLogReal
  , powUsingExpLog
  -- * Sine and cosine
  , CanSinCos(..), CanSinCosSameType, specCanSinCosReal
  , approxPi
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Numeric.CollectErrors ( CN )
import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Mul
import Numeric.MixedTypes.Field
import Numeric.MixedTypes.Power
-- import Numeric.MixedTypes.Round

import Utils.Test.EnforceRange 

{----  sqrt -----}

{-|
  A replacement for Prelude's `P.sqrt`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @sqrt@.
-}
class CanSqrt t where
  type SqrtType t
  type SqrtType t = t -- default
  sqrt :: t -> SqrtType t
  default sqrt :: (SqrtType t ~ t, P.Floating t) => t -> SqrtType t
  sqrt = P.sqrt

type CanSqrtSameType t = (CanSqrt t, SqrtType t ~ t)

{-|
  HSpec properties that each implementation of CanSqrt should satisfy.
 -}
specCanSqrtReal ::
  _ => T t -> Spec
specCanSqrtReal (T typeName :: T t) =
  describe (printf "CanSqrt %s" typeName) $ do
    it "sqrt(x) >= 0" $ do
      property $ \ (x :: t) ->
        isCertainlyNonNegative x ==>
          (sqrt x) ?>=?$ 0
    it "sqrt(x)^2 = x" $ do
      property $ \ (x :: t) ->
        isCertainlyNonNegative x ==>
          (sqrt x)^2 ?==?$ x
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  infix 4 ?>=?$
  (?>=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?>=?$) = printArgsIfFails2 "?>=?" (?>=?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanSqrt Double -- not exact, will not pass the tests

instance
  (CanSqrt a, CanTestPosNeg a, CanMinMaxThis a Integer)
  =>
  CanSqrt (CN a)
  where
  type SqrtType (CN a) = CN (SqrtType a)
  sqrt x 
    | isCertainlyNonNegative x = CN.lift sqrt x
    | isCertainlyNegative x = CN.removeValueErrorCertain sqrtx err
    | otherwise = CN.prependErrorPotential err sqrtx
    where
    sqrtx = CN.lift sqrt $ max x 0
    err :: CN.NumError
    err = CN.OutOfDomain "negative sqrt argument"


{----  exp -----}

{-|
  A replacement for Prelude's `P.exp`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @exp@.
-}
class CanExp t where
  type ExpType t
  type ExpType t = t -- default
  exp :: t -> ExpType t
  default exp :: (ExpType t ~ t, P.Floating t) => t -> ExpType t
  exp = P.exp

type CanExpSameType t = (CanExp t, ExpType t ~ t)

{-|
  HSpec properties that each implementation of CanExp should satisfy.
 -}
specCanExpReal ::
  _ => T t -> Spec
specCanExpReal (T typeName :: T t) =
  describe (printf "CanExp %s" typeName) $ do
    it "exp(x) >= 0" $ do
      property $ \ (x_ :: t) ->
        let x = enforceRange (Just (-100000), Just 100000) x_ in
          exp x ?>=?$ 0
    it "exp(-x) == 1/(exp x)" $ do
      property $ \ (x_ :: t) ->
        let x = enforceRange (Just (-100000), Just 100000) x_ in
        let ex = exp x in
          (ex !>! 0) ==>
            (exp (-x)) ?==?$ 1/ex
    it "exp(x+y) = exp(x)*exp(y)" $ do
      property $ \ (x_ :: t)  (y_ :: t) ->
        let x = enforceRange (Just (-100000), Just 100000) x_ in
        let y = enforceRange (Just (-100000), Just 100000) y_ in
          (exp $ x + y) ?==?$ (exp x) * (exp y)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  infix 4 ?>=?$
  (?>=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?>=?$) = printArgsIfFails2 "?>=?" (?>=?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanExp Double -- not exact, will not pass the tests

instance
  (CanExp a) => CanExp (CN a)
  where
  type ExpType (CN a) = CN (ExpType a)
  exp = CN.lift exp

{----  log -----}

{-|
  A replacement for Prelude's `P.log`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @log@.
-}
class CanLog t where
  type LogType t
  type LogType t = t -- default
  log :: t -> LogType t
  default log :: (LogType t ~ t, P.Floating t) => t -> LogType t
  log = P.log

type CanLogSameType t = (CanLog t, LogType t ~ t)

{-|
  HSpec properties that each implementation of CanLog should satisfy.
 -}
specCanLogReal ::
  _ => T t -> Spec
specCanLogReal (T typeName :: T t) =
  describe (printf "CanLog %s" typeName) $ do
    it "log(1/x) == -(log x)" $ do
      property $ \ (x_ :: t) ->
        let x = enforceRange (Just 0, Nothing) x_ in
        x !>! 0 && (1/x) !>! 0  ==>
          log (1/x) ?==?$ -(log x)
    it "log(x*y) = log(x)+log(y)" $ do
      property $ \ (x_ :: t)  (y_ :: t) ->
        let x = enforceRange (Just 0, Nothing) x_ in
        let y = enforceRange (Just 0, Nothing) y_ in
        x !>! 0 && y !>! 0 && x*y !>! 0  ==>
          (log $ x * y) ?==?$ (log x) + (log y)
    it "log(exp x) == x" $ do
      property $ \ (x_ :: t) ->
        let x = enforceRange (Just (-1000), Just 10000) x_ in
        let ex = exp x in
          (ex !>! 0) ==>
            log ex ?==?$ x
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanLog Double -- not exact, will not pass the tests

instance
  (CanLog a, CanTestPosNeg a)
  =>
  CanLog (CN a)
  where
  type LogType (CN a) = CN (LogType a)
  log x 
    | isCertainlyPositive x = logx
    | isCertainlyNonPositive x = CN.removeValueErrorCertain logx err
    | otherwise = CN.removeValueErrorPotential logx err
    where
    logx = CN.lift log x
    err :: CN.NumError
    err = CN.OutOfDomain "log argument not positive"


powUsingExpLog ::
  (CanLogSameType t,
   CanExpSameType t,
   CanTestInteger t,
   CanTestZero t)
  =>
   t -> (t -> t -> t) -> (t -> t) -> t -> t -> t
powUsingExpLog one mul' recip' b e =
  case certainlyIntegerGetIt e of
    Just n ->
      powUsingMulRecip one mul' recip' b n
    Nothing ->
      exp ((log b) `mul'` (e))

{----  sine and cosine -----}

{-|
  A replacement for Prelude's `P.cos` and `P.sin`.  If @Floating t@,
  then one can use the default implementation to mirror Prelude's @sin@, @cos@.
-}
class CanSinCos t where
  type SinCosType t
  type SinCosType t = t -- default
  cos :: t -> SinCosType t
  default cos :: (SinCosType t ~ t, P.Floating t) => t -> SinCosType t
  cos = P.cos
  sin :: t -> SinCosType t
  default sin :: (SinCosType t ~ t, P.Floating t) => t -> SinCosType t
  sin = P.sin

type CanSinCosSameType t = (CanSinCos t, SinCosType t ~ t)

{-|
  HSpec properties that each implementation of CanSinCos should satisfy.

  Derived partially from
  http://math.stackexchange.com/questions/1303044/axiomatic-definition-of-sin-and-cos
 -}
specCanSinCosReal ::
  _ => T t -> Spec
specCanSinCosReal (T typeName :: T t) =
  describe (printf "CanSinCos %s" typeName) $ do
    it "-1 <= sin(x) <= 1" $ do
      property $ \ (x :: t) ->
          (-1) ?<=?$ (sin x) .&&. (sin x) ?<=?$ 1
    it "-1 <= cos(x) <= 1" $ do
      property $ \ (x :: t) ->
          (-1) ?<=?$ (cos x) .&&. (cos x) ?<=?$ 1
    it "cos(x)^2 + sin(x)^2 = 1" $ do
      property $ \ (x :: t) ->
          (sin x)^2 + (cos x)^2 ?==?$ 1
    it "sin(x-y) = sin(x)cos(y) - cos(x)sin(y)" $ do
      property $ \ (x :: t) (y :: t) ->
          (sin $ x - y) ?==?$ (sin x)*(cos y) - (cos x)*(sin y)
    it "cos(x-y) = cos(x)cos(y) + sin(x)sin(y)" $ do
      property $ \ (x :: t) (y :: t) ->
          (cos $ x - y) ?==?$ (cos x)*(cos y) + (sin x)*(sin y)
    it "sin(x) < x < tan(x) for x in [0,pi/2]" $ do
      property $ \ (x :: t) ->
        x !>=! 0 && x !<=! 1.57 && (cos x) !>! 0 ==>
          (sin x) ?<=?$ x .&&. (x) ?<=?$ (sin x)/(cos x)
  where
  infix 4 ?==?$
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  infix 4 ?<=?$
  (?<=?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<=?$) = printArgsIfFails2 "?<=?" (?<=?)

{-
  Instances for Integer, Rational etc need an algebraic real or exact real type.
  Such type is not provided in this package. See eg aern2-real.
-}

instance CanSinCos Double -- not exact, will not pass the tests

instance
  (CanSinCos a) => CanSinCos (CN a)
  where
  type SinCosType (CN a) = CN (SinCosType a)
  sin = CN.lift sin
  cos = CN.lift cos

{-|
  Approximate pi, synonym for Prelude's `P.pi`.

  We do not define (exect) @pi@ in this package as we have no type
  that can represent it exactly.
-}
approxPi :: (P.Floating t) => t
approxPi = P.pi
