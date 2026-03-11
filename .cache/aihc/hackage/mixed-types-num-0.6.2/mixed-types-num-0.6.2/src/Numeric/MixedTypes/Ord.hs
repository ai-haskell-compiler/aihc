{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Ord
    Description :  Bottom-up typed order comparisons
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Ord
(
  -- * Comparisons in numeric order
  HasOrder, HasOrderAsymmetric(..), (>), (<), (<=), (>=)
  , HasOrderCertainlyAsymmetric, HasOrderCertainly
  , (?<=?), (?<?), (?>=?), (?>?)
  , (!<=!), (!<!), (!>=!), (!>!)
  -- ** Tests
  , specHasOrder, specHasOrderNotMixed
  -- ** Specific comparisons
  , CanTestPosNeg(..)
)
where

import Utils.TH.DeclForTypes

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import Test.Hspec
import qualified Test.QuickCheck as QC

import Control.CollectErrors ( CollectErrors, CanBeErrors )
import qualified Control.CollectErrors as CE

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
-- import Numeric.MixedTypes.Eq

infix  4  <, <=, >=, >
infix 4 ?<=?, ?<?, ?>=?, ?>?
infix 4 !<=!, !<!, !>=!, !>!

{---- Inequality -----}

type HasOrder t1 t2 =
  (HasOrderAsymmetric t1 t2, HasOrderAsymmetric t2 t1,
   OrderCompareType t1 t2 ~ OrderCompareType t2 t1)

type HasOrderCertainly t1 t2 =
  (HasOrder t1 t2, CanTestCertainly (OrderCompareType t1 t2))

type HasOrderCertainlyAsymmetric t1 t2 =
  (HasOrderAsymmetric t1 t2, CanTestCertainly (OrderCompareType t1 t2))

class (IsBool (OrderCompareType a b)) => HasOrderAsymmetric a b where
    type OrderCompareType a b
    type OrderCompareType a b = Bool -- default
    lessThan :: a -> b -> (OrderCompareType a b)
    -- default lessThan via Prelude for Bool:
    default lessThan :: (OrderCompareType a b ~ Bool, a~b, P.Ord a) => a -> b -> OrderCompareType a b
    lessThan = (P.<)
    greaterThan :: a -> b -> (OrderCompareType a b)
    default greaterThan ::
      (HasOrder b a, OrderCompareType b a ~ OrderCompareType a b) =>
      a -> b -> (OrderCompareType a b)
    greaterThan a b = lessThan b a
    leq :: a -> b -> (OrderCompareType a b)
    -- default lessThan via Prelude for Bool:
    default leq :: (OrderCompareType a b ~ Bool, a~b, P.Ord a) => a -> b -> OrderCompareType a b
    leq = (P.<=)
    geq :: a -> b -> (OrderCompareType a b)
    default geq ::
      (HasOrder b a, OrderCompareType b a ~ OrderCompareType a b) =>
      a -> b -> (OrderCompareType a b)
    geq a b = leq b a

(>) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(>) = greaterThan
(<) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(<) = lessThan

(>=) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(>=) = geq
(<=) :: (HasOrderAsymmetric a b) => a -> b -> OrderCompareType a b
(<=) = leq

(?>?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?>? b = isNotFalse $ a > b

(?<?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?<? b = isNotFalse $ a < b

(?>=?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?>=? b = isNotFalse $ a >= b

(?<=?) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a ?<=? b = isNotFalse $ a <= b

(!>!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !>! b = isCertainlyTrue $ a > b

(!<!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !<! b = isCertainlyTrue $ a < b

(!>=!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !>=! b = isCertainlyTrue $ a >= b

(!<=!) :: (HasOrderCertainlyAsymmetric a b) => a -> b -> Bool
a !<=! b = isCertainlyTrue $ a <= b

{-|
  HSpec properties that each implementation of 'HasOrder' should satisfy.
 -}
specHasOrder ::
  _ => T t1 -> T t2 -> T t3 -> Spec
specHasOrder (T typeName1 :: T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "HasOrd %s %s, HasOrd %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "has reflexive >=" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyFalse (x >= x)
    it "has reflexive <=" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyFalse (x <= x)
    it "has anti-reflexive >" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyTrue (x > x)
    it "has anti-reflexive <" $ do
      QC.property $ \ (x :: t1) -> not $ isCertainlyTrue (x < x)
    it "> stronly implies >=" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x > y) `stronglyImplies` (x >= y)
    it "< stronly implies <=" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x < y) `stronglyImplies` (x <= y)
    it "has stronly equivalent > and <" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x < y) `stronglyEquivalentTo` (y > x)
    it "has stronly equivalent >= and <=" $ do
      QC.property $ \ (x :: t1) (y :: t2) -> (x <= y) `stronglyEquivalentTo` (y >= x)
    it "has stronly transitive <" $ do
      QC.property $ \ (x :: t1) (y :: t2) (z :: t3) -> ((x < y) && (y < z)) `stronglyImplies` (y < z)

{-|
  HSpec properties that each implementation of 'HasOrder' should satisfy.
 -}
specHasOrderNotMixed ::
  _ => T t -> Spec
specHasOrderNotMixed (t :: T t) = specHasOrder t t t

instance HasOrderAsymmetric () () where
  lessThan _ _ = False
  leq _ _ = True

instance HasOrderAsymmetric Int Int
instance HasOrderAsymmetric Integer Integer
instance HasOrderAsymmetric Rational Rational
instance HasOrderAsymmetric Double Double

instance HasOrderAsymmetric Int Integer where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Integer Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Int Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Rational Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Integer Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Rational Integer where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Integer Double where
  lessThan n d = (n <= (P.floor d :: Integer)) && (n < (P.ceiling d :: Integer))
  leq n d = (n <= (P.floor d :: Integer))
instance HasOrderAsymmetric Double Integer where
  lessThan d n = ((P.floor d :: Integer) < n) && ((P.ceiling d :: Integer) <= n)
  leq d n = ((P.ceiling d :: Integer) <= n)

instance HasOrderAsymmetric Int Double where
  lessThan n d = lessThan (integer n) d
  leq n d = leq (integer n) d
instance HasOrderAsymmetric Double Int where
  lessThan d n = lessThan d (integer n)
  leq d n = leq d (integer n)

instance
  (HasOrderAsymmetric a b, CanBeErrors es, CanTestCertainly (OrderCompareType a b))
  =>
  HasOrderAsymmetric (CollectErrors es a) (CollectErrors es b)
  where
  type OrderCompareType (CollectErrors es a) (CollectErrors es b) =
    CollectErrors es (OrderCompareType a b)
  lessThan = CE.lift2 lessThan
  leq = CE.lift2 leq
  greaterThan = CE.lift2 greaterThan
  geq = CE.lift2 geq

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance
      (HasOrderAsymmetric $t b, CanBeErrors es, CanTestCertainly (OrderCompareType $t b))
      =>
      HasOrderAsymmetric $t (CollectErrors es  b)
      where
      type OrderCompareType $t (CollectErrors es  b) =
        CollectErrors es (OrderCompareType $t b)
      lessThan = CE.liftT1 lessThan
      leq = CE.liftT1 leq
      greaterThan = CE.liftT1 greaterThan
      geq = CE.liftT1 geq

    instance
      (HasOrderAsymmetric a $t, CanBeErrors es, CanTestCertainly (OrderCompareType a $t))
      =>
      HasOrderAsymmetric (CollectErrors es a) $t
      where
      type OrderCompareType (CollectErrors es  a) $t =
        CollectErrors es (OrderCompareType a $t)
      lessThan = CE.lift1T lessThan
      leq = CE.lift1T leq
      greaterThan = CE.lift1T greaterThan
      geq = CE.lift1T geq

  |]))

class CanTestPosNeg t where
    isCertainlyPositive :: t -> Bool
    isCertainlyNonNegative :: t -> Bool
    isCertainlyNegative :: t -> Bool
    isCertainlyNonPositive :: t -> Bool
    default isCertainlyPositive :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyPositive a = isCertainlyTrue $ a > 0
    default isCertainlyNonNegative :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyNonNegative a = isCertainlyTrue $ a >= 0
    default isCertainlyNegative :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyNegative a = isCertainlyTrue $ a < 0
    default isCertainlyNonPositive :: (HasOrderCertainly t Integer) => t -> Bool
    isCertainlyNonPositive a = isCertainlyTrue $ a <= 0

instance CanTestPosNeg Int
instance CanTestPosNeg Integer
instance CanTestPosNeg Rational
instance CanTestPosNeg Double

instance (CanTestPosNeg t, CanBeErrors es) => (CanTestPosNeg (CollectErrors es t)) where
  isCertainlyPositive = CE.withErrorOrValue (const False) isCertainlyPositive
  isCertainlyNonNegative = CE.withErrorOrValue (const False) isCertainlyNonNegative
  isCertainlyNegative = CE.withErrorOrValue (const False) isCertainlyNegative
  isCertainlyNonPositive = CE.withErrorOrValue (const False) isCertainlyNonPositive
