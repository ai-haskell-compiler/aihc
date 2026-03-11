{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.AddSub
    Description :  Bottom-up typed addition and subtraction
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.AddSub
(
    -- * Addition
    CanAdd, CanAddAsymmetric(..), CanAddThis, CanAddSameType
    , (+), sum, sumWithSample
  -- ** Tests
    , specCanAdd, specCanAddNotMixed, specCanAddSameType
    -- * Subtraction
    , CanSub(..), CanSubThis, CanSubSameType
    , (-)
  -- ** Tests
    , specCanSub, specCanSubNotMixed
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Control.CollectErrors ( CollectErrors, CanBeErrors )
import qualified Control.CollectErrors as CE

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
import Numeric.MixedTypes.MinMaxAbs ()

{---- Addition -----}

type CanAdd t1 t2 =
  (CanAddAsymmetric t1 t2, CanAddAsymmetric t2 t1,
   AddType t1 t2 ~ AddType t2 t1)

{-|
  A replacement for Prelude's `P.+`.  If @t1 = t2@ and @Num t1@,
  then one can use the default implementation to mirror Prelude's @+@.
-}
class CanAddAsymmetric t1 t2 where
  type AddType t1 t2
  type AddType t1 t2 = t1 -- default
  add :: t1 -> t2 -> AddType t1 t2
  default add :: (AddType t1 t2 ~ t1, t1~t2, P.Num t1) => t1 -> t2 -> AddType t1 t2
  add = (P.+)

infixl 6  +, -

(+) :: (CanAddAsymmetric t1 t2) => t1 -> t2 -> AddType t1 t2
(+) = add

(-) :: (CanSub t1 t2) => t1 -> t2 -> SubType t1 t2
(-) = sub

type CanAddThis t1 t2 =
  (CanAdd t1 t2, AddType t1 t2 ~ t1)
type CanAddSameType t =
  CanAddThis t t

sum :: (CanAddSameType t, ConvertibleExactly Integer t) => [t] -> t
sum xs = List.foldl' add (convertExactly 0) xs

sumWithSample :: (CanAddSameType t, ConvertibleExactlyWithSample Integer t) => t -> [t] -> t
sumWithSample sampleT xs = List.foldl' add (convertExactlyWithSample sampleT 0) xs

{-|
  HSpec properties that each implementation of CanAdd should satisfy.
 -}
specCanAdd ::
  _ => T t1 -> T t2 -> T t3 -> Spec
specCanAdd (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanAdd %s %s, CanAdd %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "absorbs 0" $ do
      property $ \ (x :: t1) (sampleT2 :: t2) -> 
        let z = (convertExactlyWithSample sampleT2 0 :: t2) in (x + z) ?==?$ x
    it "is commutative" $ do
      property $ \ (x :: t1) (y :: t2) -> (x + y) ?==?$ (y + x)
    it "is associative" $ do
      property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x + (y + z)) ?==?$ ((x + y) + z)
    it "increases when positive" $ do
      property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyPositive x) ==> (x + y) ?>?$ y
    it "decreases when negative" $ do
      property $ \ (x :: t1) (y :: t2) ->
        (isCertainlyNegative x) ==> (x + y) ?<?$ y
  where
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)
  (?>?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?>?$) = printArgsIfFails2 "?>?" (?>?)
  (?<?$) :: (HasOrderCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?<?$) = printArgsIfFails2 "?<?" (?<?)

--
{-|
  HSpec properties that each implementation of CanAdd should satisfy.
 -}
specCanAddNotMixed ::
  _ => T t -> Spec
specCanAddNotMixed (t :: T t) = specCanAdd t t t

{-|
  HSpec properties that each implementation of CanAddSameType should satisfy.
 -}
specCanAddSameType ::
  (ConvertibleExactlyWithSample Integer t, Show t,
   HasEqCertainly t t, CanAddSameType t, Arbitrary t)
   =>
   T t -> Spec
specCanAddSameType (T typeName :: T t) =
  describe (printf "CanAddSameType %s" typeName) $ do
    it "has sum working over integers" $ do
      property $ \ (xsi :: [Integer]) (sampleT :: t) ->
        (sumWithSample sampleT $ (map (convertExactlyWithSample sampleT) xsi :: [t])) 
        ?==?$ 
        (convertExactlyWithSample sampleT (sum xsi) :: t)
    it "has sum [] = 0" $ do
      property $ \ (sampleT :: t) ->
        (sumWithSample sampleT ([] :: [t])) ?==?$ (convertExactlyWithSample sampleT 0 :: t)
  where
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

instance CanAddAsymmetric Int Int where
  type AddType Int Int = Integer -- do not risk overflow
  add a b = (integer a) P.+ (integer b)

instance CanAddAsymmetric Integer Integer
instance CanAddAsymmetric Rational Rational
instance CanAddAsymmetric Double Double

instance CanAddAsymmetric Int Integer where
  type AddType Int Integer = Integer
  add = convertFirst add
instance CanAddAsymmetric Integer Int where
  type AddType Integer Int = Integer
  add = convertSecond add

instance CanAddAsymmetric Int Rational where
  type AddType Int Rational = Rational
  add = convertFirst add
instance CanAddAsymmetric Rational Int where
  type AddType Rational Int = Rational
  add = convertSecond add

instance CanAddAsymmetric Integer Rational where
  type AddType Integer Rational = Rational
  add = convertFirst add
instance CanAddAsymmetric Rational Integer where
  type AddType Rational Integer = Rational
  add = convertSecond add

instance CanAddAsymmetric Int Double where
  type AddType Int Double = Double
  add n d = add (double n) d
instance CanAddAsymmetric Double Int where
  type AddType Double Int = Double
  add d n = add d (double n)

instance CanAddAsymmetric Integer Double where
  type AddType Integer Double = Double
  add n d = add (double n) d
instance CanAddAsymmetric Double Integer where
  type AddType Double Integer = Double
  add d n = add d (double n)

instance CanAddAsymmetric Rational Double where
  type AddType Rational Double = Double
  add n d = add (double n) d
instance CanAddAsymmetric Double Rational where
  type AddType Double Rational = Double
  add d n = add d (double n)

instance (CanAddAsymmetric a b) => CanAddAsymmetric [a] [b] where
  type AddType [a] [b] = [AddType a b]
  add (x:xs) (y:ys) = (add x y) : (add xs ys)
  add _ _ = []

instance (CanAddAsymmetric a b) => CanAddAsymmetric (Maybe a) (Maybe b) where
  type AddType (Maybe a) (Maybe b) = Maybe (AddType a b)
  add (Just x) (Just y) = Just (add x y)
  add _ _ = Nothing

instance
  (CanAddAsymmetric a b, CanBeErrors es)
  =>
  CanAddAsymmetric (CollectErrors es a) (CollectErrors es  b)
  where
  type AddType (CollectErrors es a) (CollectErrors es b) =
    CollectErrors es (AddType a b)
  add = CE.lift2 add

-- TH for ground type instances at is the end of the file due to a bug in TH

{---- Subtraction -----}

{-|
  A replacement for Prelude's binary `P.-`.

  If @CanNeg t2@ and @CanAdd t1 (NegType t2)@,
  then one can use the default implementation
  via @a-b = a + (-b)@.
-}
class CanSub t1 t2 where
  type SubType t1 t2
  type SubType t1 t2 = AddType t1 (NegType t2) -- default
  sub :: t1 -> t2 -> SubType t1 t2
  default sub ::
    (SubType t1 t2 ~ AddType t1 (NegType t2),
    CanNeg t2, CanAdd t1 (NegType t2))
    =>
    t1 -> t2 -> SubType t1 t2
  a `sub` b = a + (negate b)

type CanSubThis t1 t2 =
  (CanSub t1 t2, SubType t1 t2 ~ t1)
type CanSubSameType t =
  CanSubThis t t

{-|
  HSpec properties that each implementation of CanSub should satisfy.
 -}
specCanSub ::
  _ => T t1 -> T t2 -> Spec
specCanSub (T typeName1 :: T t1) (T typeName2 :: T t2) =
  describe (printf "CanSub %s %s" typeName1 typeName2) $ do
    it "x-0 = x" $ do
      property $ \ (x :: t1) (sampleT2 :: t2) -> 
        let z = (convertExactlyWithSample sampleT2 0 :: t2) in (x - z) ?==?$ x
    it "x-x = 0" $ do
      property $ \ (x :: t1) (sampleR :: SubType t1 t1) -> 
        let z = (convertExactlyWithSample sampleR 0 :: SubType t1 t1) in (x - x) ?==?$ z
    it "x-y = x+(-y)" $ do
      property $ \ (x :: t1) (y :: t2) ->
        (x - y) ?==?$ (x + (negate y))
  where
  (?==?$) :: (HasEqCertainlyAsymmetric a b, Show a, Show b) => a -> b -> Property
  (?==?$) = printArgsIfFails2 "?==?" (?==?)

--
{-|
  HSpec properties that each implementation of CanSub should satisfy.
 -}
specCanSubNotMixed ::
  _ => T t -> Spec
specCanSubNotMixed (t :: T t) = specCanSub t t

instance CanSub Int Int where
  type SubType Int Int = Integer -- do not risk overflow
  sub a b = (integer a) P.- (integer b)

instance CanSub Integer Integer
instance CanSub Rational Rational
instance CanSub Double Double

instance CanSub Int Integer where
  type SubType Int Integer = Integer
  sub = convertFirst sub
instance CanSub Integer Int where
  type SubType Integer Int = Integer
  sub = convertSecond sub

instance CanSub Int Rational where
  type SubType Int Rational = Rational
  sub = convertFirst sub
instance CanSub Rational Int where
  type SubType Rational Int = Rational
  sub = convertSecond sub

instance CanSub Integer Rational where
  type SubType Integer Rational = Rational
  sub = convertFirst sub
instance CanSub Rational Integer where
  type SubType Rational Integer = Rational
  sub = convertSecond sub

instance CanSub Int Double where
  type SubType Int Double = Double
  sub n d = sub (double n) d
instance CanSub Double Int where
  type SubType Double Int = Double
  sub d n = sub d (double n)

instance CanSub Integer Double where
  type SubType Integer Double = Double
  sub n d = sub (double n) d
instance CanSub Double Integer where
  type SubType Double Integer = Double
  sub d n = sub d (double n)

instance CanSub Rational Double where
  type SubType Rational Double = Double
  sub n d = sub (double n) d
instance CanSub Double Rational where
  type SubType Double Rational = Double
  sub d n = sub d (double n)

instance (CanSub a b) => CanSub [a] [b] where
  type SubType [a] [b] = [SubType a b]
  sub (x:xs) (y:ys) = (sub x y) : (sub xs ys)
  sub _ _ = []

instance (CanSub a b) => CanSub (Maybe a) (Maybe b) where
  type SubType (Maybe a) (Maybe b) = Maybe (SubType a b)
  sub (Just x) (Just y) = Just (sub x y)
  sub _ _ = Nothing


instance
  (CanSub a b, CanBeErrors es)
  =>
  CanSub (CollectErrors es a) (CollectErrors es  b)
  where
  type SubType (CollectErrors es a) (CollectErrors es b) =
    CollectErrors es (SubType a b)
  sub = CE.lift2 sub

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (CanSub $t b, CanBeErrors es)
      =>
      CanSub $t (CollectErrors es  b)
      where
      type SubType $t (CollectErrors es  b) =
        CollectErrors es (SubType $t b)
      sub = CE.liftT1 sub

    instance
      (CanSub a $t, CanBeErrors es)
      =>
      CanSub (CollectErrors es a) $t
      where
      type SubType (CollectErrors es  a) $t =
        CollectErrors es (SubType a $t)
      sub = CE.lift1T sub

    instance
      (CanAddAsymmetric $t b, CanBeErrors es)
      =>
      CanAddAsymmetric $t (CollectErrors es  b)
      where
      type AddType $t (CollectErrors es  b) =
        CollectErrors es (AddType $t b)
      add = CE.liftT1 add

    instance
      (CanAddAsymmetric a $t, CanBeErrors es)
      =>
      CanAddAsymmetric (CollectErrors es a) $t
      where
      type AddType (CollectErrors es  a) $t =
        CollectErrors es (AddType a $t)
      add = CE.lift1T add
  |]))
