{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  Numeric.MixedType.Bool
    Description :  Bottom-up typed Boolean operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}
module Numeric.MixedTypes.Bool
(
  IsBool, specIsBool
  -- * Conversion to/from Bool
  , HasBools, CanTestCertainly(..), specCanTestCertainly, CanTestCertainlyX
  , isNotTrue, isNotFalse
  , stronglyImplies, stronglyEquivalentTo
  , weaklyImplies, weaklyEquivalentTo
  -- * Negation
  , CanNeg(..), not, CanNegSameType
  -- ** Tests
  , specCanNegBool
  -- * And and or
  , CanAndOr, CanAndOrAsymmetric(..), (&&), (||), CanAndOrWith, CanAndOrSameType, and, or
  -- ** Tests
  , specCanAndOr, specCanAndOrNotMixed
)
where

import Numeric.MixedTypes.PreludeHiding
import qualified Prelude as P
import Text.Printf

import qualified Data.List as List

import Control.CollectErrors ( CollectErrors, CanBeErrors )
import qualified Control.CollectErrors as CE

import Numeric.MixedTypes.Literals

import Test.Hspec
-- import qualified Test.QuickCheck as QC
import qualified Test.Hspec.SmallCheck as HSC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SCS
-- import Control.Exception (evaluate)

type HasBools t = (ConvertibleExactly Bool t)

{-|
  Tests for truth or falsity.  Beware, when @isCertainlyTrue@ returns @False@,
  it does not mean that the proposition is false.  It usually means that
  we failed to prove the proposition.
-}
class (HasBools t) => CanTestCertainly t
  where
    isCertainlyTrue :: t -> Bool
    isCertainlyFalse :: t -> Bool

isNotFalse :: (CanTestCertainly t) => t -> Bool
isNotFalse = P.not . isCertainlyFalse

isNotTrue :: (CanTestCertainly t) => t -> Bool
isNotTrue = P.not . isCertainlyTrue

{-|
  If l is certainly True, then r is also certainly True.
-}
stronglyImplies :: (CanTestCertainly t1, CanTestCertainly t2) => t1 -> t2 -> Bool
stronglyImplies l r =
  (P.not (isCertainlyTrue l) P.|| isCertainlyTrue r)

{-|
  If l is certainly True, then r is not certainly False.
-}
weaklyImplies :: (CanTestCertainly t1, CanTestCertainly t2) => t1 -> t2 -> Bool
weaklyImplies l r =
  (P.not $ isCertainlyTrue l) P.|| (P.not $ isCertainlyFalse r)

stronglyEquivalentTo :: (CanTestCertainly t1, CanTestCertainly t2) => t1 -> t2 -> Bool
stronglyEquivalentTo l r =
  stronglyImplies l r P.&& stronglyImplies r l

weaklyEquivalentTo :: (CanTestCertainly t1, CanTestCertainly t2) => t1 -> t2 -> Bool
weaklyEquivalentTo l r =
  weaklyImplies l r P.&& weaklyImplies r l

{-|
  HSpec properties that each implementation of CanTestCertainly should satisfy.
 -}
specCanTestCertainly :: (CanTestCertainly t) => T t -> Spec
specCanTestCertainly (T typeName :: T t) =
  describe (printf "CanTestCertainly %s" typeName) $ do
    it "detects True using isCertainlyTrue" $ do
      isCertainlyTrue (convertExactly True :: t) `shouldBe`  True
    it "does not detect False using isCertainlyTrue" $ do
      isCertainlyTrue (convertExactly False :: t) `shouldBe`  False
    it "detects False using isCertainlyFalse" $ do
      isCertainlyFalse (convertExactly False :: t) `shouldBe`  True
    it "does not detect True using isCertainlyFalse" $ do
      isCertainlyFalse (convertExactly True :: t) `shouldBe`  False

instance ConvertibleExactly Bool Bool where
  safeConvertExactly b = Right b

instance CanTestCertainly Bool where
  isCertainlyTrue = id
  isCertainlyFalse = not

instance (CanTestCertainly t, CanBeErrors es) => CanTestCertainly (CollectErrors es t) where
  isCertainlyTrue vCE =
    CE.withErrorOrValue (const False) isCertainlyTrue vCE
  isCertainlyFalse vCE =
    CE.withErrorOrValue (const False) isCertainlyFalse vCE


{---- Negation ----}

{-|
  This is negation is both the numeric negation as well as the Boolean negation.
  Example of non-standard Boolean negation:

  @
  negate (Just True) = Just False
  @
 -}
class CanNeg t where
  type NegType t
  type NegType t = t
  negate :: t -> NegType t

{-| A synonym of 'negate'. -}
not :: (CanNeg t) => t -> NegType t
not = negate

type CanNegSameType t =
  (CanNeg t, NegType t ~ t)

{-| Compound type constraint useful for test definition. -}
type CanTestCertainlyX t = (CanTestCertainly t, Show t, SCS.Serial IO t)

{-|
  HSpec properties that each Boolean implementation of CanNeg should satisfy.
 -}
specCanNegBool ::
  _ => T t -> Spec
specCanNegBool (T typeName :: T t) =
  describe (printf "CanNeg %s" typeName) $ do
    it "ignores double negation" $ do
      HSC.property $ \ (x :: t) -> (not (not x)) `scEquals` x
    it "negates True to False" $ do
      HSC.property $ \ (x :: t) ->
        (isCertainlyTrue x) SC.==> (isCertainlyFalse (not x))
    it "negates False to True" $ do
      HSC.property $ \ (x :: t) ->
        (isCertainlyFalse x) SC.==> (isCertainlyTrue (not x))


instance CanNeg Bool where negate = P.not

instance
  (CanNeg t, CanBeErrors es)
  =>
  CanNeg (CollectErrors es t) where
  type NegType (CollectErrors es t) = CollectErrors es (NegType t)
  negate = fmap negate

{---- And/Or ----}

type CanAndOr t1 t2 =
  (CanAndOrAsymmetric t1 t2, CanAndOrAsymmetric t2 t1,
   AndOrType t1 t2 ~ AndOrType t2 t1)

{-|
  Binary logical `and' and `or' for generalised Booleans.  For example:

  @
  (Just True) && False = Just False
  (Just (Just True)) || False = (Just (Just True))
  @
 -}
class CanAndOrAsymmetric t1 t2 where
  type AndOrType t1 t2
  and2 :: t1 -> t2 -> AndOrType t1 t2
  or2 :: t1 -> t2 -> AndOrType t1 t2

type CanAndOrWith t1 t2 = (CanAndOr t1 t2, AndOrType t1 t2 ~ t1)
type CanAndOrSameType t = (CanAndOrWith t t)

infixr 3  &&
infixr 2  ||

{-| A synonym of 'and2'. -}
(&&) :: (CanAndOrAsymmetric a b) => a -> b -> AndOrType a b
(&&) = and2
{-| A synonym of 'or2'. -}
(||) :: (CanAndOrAsymmetric a b) => a -> b -> AndOrType a b
(||) = or2

and :: (CanAndOrSameType t, CanTestCertainly t) => [t] -> t
and = List.foldl' (&&) (convertExactly True)

or :: (CanAndOrSameType t, CanTestCertainly t) => [t] -> t
or = List.foldl' (||) (convertExactly False)

{-|
  HSpec properties that each implementation of CanAndOr should satisfy.
 -}
specCanAndOr :: _ => T t1 -> T t2 -> T t3 -> Spec
specCanAndOr (T typeName1 ::T t1) (T typeName2 :: T t2) (T typeName3 :: T t3) =
  describe (printf "CanAndOr %s %s, CanAndOr %s %s" typeName1 typeName2 typeName2 typeName3) $ do
    it "has idempotent ||" $ do
      HSC.property $ \ (x :: t1) -> (x || x) `scEquals` x
    it "has idempotent &&" $ do
      HSC.property $ \ (x :: t1) -> (x && x) `scEquals` x
    it "has commutative ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (x || y) `scEquals` (y || x)
    it "has commutative &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (x && y) `scEquals` (y && x)
    it "has associative ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x || (y || z)) `scEquals` ((x || y) || z)
    it "has associative &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x && (y && z)) `scEquals` ((x && y) && z)
    it "distributes || over &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x || (y && z)) `scEquals` ((x || y) && (x || z))
    it "distributes && over ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) (z :: t3) ->
                      (x && (y || z)) `scEquals` ((x && y) || (x && z))
    it "distributes not over ||" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (not (x || y)) `scEquals` ((not x) && (not y))
    it "distributes not over &&" $ do
      HSC.property $ \ (x :: t1) (y :: t2) -> (not (x && y)) `scEquals` ((not x) || (not y))

{-|
  HSpec properties that each implementation of CanAndOr should satisfy.
 -}
specCanAndOrNotMixed :: _ => T t -> Spec
specCanAndOrNotMixed t = specCanAndOr t t t

instance CanAndOrAsymmetric Bool Bool where
  type AndOrType Bool Bool = Bool
  and2 = (P.&&)
  or2 = (P.||)

instance
  (CanAndOrAsymmetric t1 t2, CanBeErrors es,
  HasBools t2, CanTestCertainly t1)
  =>
  CanAndOrAsymmetric (CollectErrors es t1) (CollectErrors es t2)
  where
  type AndOrType (CollectErrors es t1) (CollectErrors es t2) = CollectErrors es (AndOrType t1 t2)
  and2 b1CE (b2CE::b2T) = 
    case b1CE of
      CE.CollectErrors (Just b1) _ | isCertainlyFalse b1 -> 
        CE.lift2 and2 b1CE (convertExactly False :: b2T) -- avoid evaluating b2CE
      _ -> CE.lift2 and2 b1CE b2CE
  or2 b1CE (b2CE::b2T) = 
    case b1CE of
      CE.CollectErrors (Just b1) _ | isCertainlyTrue b1 -> 
        CE.lift2 or2 b1CE (convertExactly True :: b2T) -- avoid evaluating b2CE
      _ -> CE.lift2 or2 b1CE b2CE

instance
  (CanAndOrAsymmetric t1 Bool, CanBeErrors es)
  =>
  CanAndOrAsymmetric (CollectErrors es t1) Bool
  where
  type AndOrType (CollectErrors es t1) Bool = CollectErrors es (AndOrType t1 Bool)
  and2 = CE.lift1T and2
  or2 = CE.lift1T or2

instance
  (CanAndOrAsymmetric Bool t2, CanBeErrors es)
  =>
  CanAndOrAsymmetric Bool (CollectErrors es t2)
  where
  type AndOrType Bool (CollectErrors es t2) = CollectErrors es (AndOrType Bool t2)
  and2 = CE.liftT1 and2
  or2 = CE.liftT1 or2

{-|
  A type constraint synonym that stipulates that the type behaves very
  much like Bool, except it does not necessarily satisfy the law of excluded middle,
  which means that the type can contain a "do-not-know" value or an error.

  Examples: @Bool@, @Kleenean@, @CollectErrors Bool@
-}
type IsBool t =
  (HasBools t, CanNegSameType t, CanAndOrSameType t)

{-|
  HSpec properties that each implementation of IsBool should satisfy.
 -}
specIsBool :: (IsBool t, CanTestCertainly t, Show t, SCS.Serial IO t) => T t -> Spec
specIsBool t@(T typeName :: T t) =
  describe (printf "IsBool %s" typeName) $ do
    specCanTestCertainly t
    specCanNegBool t
    specCanAndOrNotMixed t

scEquals ::
  (Show t1, CanTestCertainly t1, Show t2, CanTestCertainly t2) =>
  t1 -> t2 -> Either String String
scEquals l r
  | l `stronglyEquivalentTo` r = Right "OK"
  | otherwise = Left $ printf "(%s) /= (%s)" (show l) (show r)
