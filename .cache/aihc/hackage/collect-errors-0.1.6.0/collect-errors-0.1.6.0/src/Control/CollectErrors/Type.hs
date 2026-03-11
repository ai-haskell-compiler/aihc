{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.CollectErrors.Type where

import Prelude

import Control.Applicative ( Applicative(liftA2), liftA )

import GHC.Generics
import Control.DeepSeq

import qualified Data.Set as Set

import Test.QuickCheck ( Arbitrary(arbitrary) )

import Text.Printf ( printf )

{-|
  A wrapper around values which can accommodate a list of
  (potential) errors that have (maybe) occurred during the computation
  of a value.  A value may be missing, leaving only the error(s).

  Such error collection allows one to write expressions with partial
  functions (ie functions that fail for some inputs) instead of
  branching after each application of such function.
  Dealing with the errors can be moved outside the expression.
  If the error data contain enough information, their list can be used
  to trace the source of the errors.
-}
data CollectErrors es v =
  CollectErrors
    { getMaybeValue :: Maybe v
    , getErrors :: es }
  deriving (Generic, Generic1, NFData, NFData1)

class CanTestErrorsCertain es where
  hasCertainError :: es -> Bool

instance (CanTestErrorsCertain es) => CanTestErrorsCertain (CollectErrors es v) where
  hasCertainError (CollectErrors _ es) = hasCertainError es

instance (CanTestErrorsCertain es) => CanTestErrorsCertain [es] where
  hasCertainError = or . map hasCertainError

instance (CanTestErrorsCertain es) => CanTestErrorsCertain (Set.Set es) where
  hasCertainError = or . map hasCertainError . Set.toList

class CanTestErrorsPresent es where
  hasError :: es -> Bool

instance (CanTestErrorsPresent es) => CanTestErrorsPresent (CollectErrors es v) where
  hasError (CollectErrors _ es) = hasError es

instance CanTestErrorsPresent [es] where
  hasError = not . null

instance CanTestErrorsPresent (Set.Set es) where
  hasError = not . null

type CanBeErrors es = (Monoid es, Eq es, Show es, CanTestErrorsCertain es, CanTestErrorsPresent es)

instance (Show v, CanBeErrors es) => (Show (CollectErrors es v)) where
  show (CollectErrors mv es) =
    case mv of
      Just v | es == mempty -> show v
      Just v -> printf "%s{%s}" (show v) (show es)
      Nothing -> printf "{%s}" (show es)

noValue :: es -> CollectErrors es v
noValue es = CollectErrors Nothing es

{-| Take a CE-value, add new errors into it and remove the value, if any -}
removeValue :: Monoid es => CollectErrors es v -> es -> CollectErrors es v
removeValue (CollectErrors _ es1) es2 =
  CollectErrors Nothing (es1 <> es2)

prependErrors :: (Monoid es) => es -> CollectErrors es v -> CollectErrors es v
prependErrors es1 (CollectErrors mv es2) = CollectErrors mv (es1 <> es2)

{-| Unsafe way to get a value out of the CollectErrors wrapper. -}
unCollectErrors :: Show es => CollectErrors es p -> p
unCollectErrors (CollectErrors (Just v) _) = v
unCollectErrors (CollectErrors _ es) = error $ "CollectErrors: " ++ show es

{-| Unsafe way to get a value out of the CollectErrors wrapper. -}
(~!) :: Show es => CollectErrors es p -> p
(~!) = unCollectErrors

{-| A safe way to get a value out of the CollectErrors wrapper. -}
toEither ::
  (CanBeErrors es)
  =>
  CollectErrors es v -> Either es v
toEither (CollectErrors mv es) =
  case mv of
    Just v | es == mempty -> Right v
    _ -> Left es

withErrorOrValue :: 
  (CanBeErrors es)
  =>
  (es -> t) -> (v -> t) -> CollectErrors es v -> t
withErrorOrValue onError onValue (CollectErrors mv es) =
  case mv of
    Just v | es == mempty -> onValue v
    _ -> onError es

filterValuesWithoutError ::
  (CanBeErrors es)
  =>
  [CollectErrors es v] -> [v]
filterValuesWithoutError [] = []
filterValuesWithoutError (vCE : rest) =
  withErrorOrValue (const restDone) (: restDone) vCE
  where
  restDone = filterValuesWithoutError rest



-- functor instances:

instance Functor (CollectErrors es) where
  fmap f (CollectErrors mv es) =
    CollectErrors (fmap f mv) es

instance (Monoid es) => Applicative (CollectErrors es) where
  pure v = CollectErrors (Just v) mempty
  (CollectErrors (Just a) ae) <*> (CollectErrors (Just b) be) =
    CollectErrors (Just (a b)) (ae <> be)
  (CollectErrors _ ae) <*> (CollectErrors _ be) =
    CollectErrors Nothing (ae <> be)

instance (Monoid es) => Monad (CollectErrors es) where
  ae >>= f =
    case ae of
      CollectErrors (Just a) es1 ->
        let (CollectErrors mv es2) = f a in
          CollectErrors mv (es1 <> es2)
      CollectErrors _ es ->
        CollectErrors Nothing es

instance (Arbitrary t, Monoid es) => Arbitrary (CollectErrors es t) where
  arbitrary = (\v -> CollectErrors (Just v) mempty) <$> arbitrary

-- Various lifts:

lift :: (Monoid es) => (a -> b) -> (CollectErrors es a) -> (CollectErrors es b)
lift = liftA

liftCE :: (Monoid es) => (a -> (CollectErrors es c)) -> (CollectErrors es a) -> (CollectErrors es c)
liftCE f (CollectErrors (Just a) ae) =
  prependErrors ae $ f a
liftCE _ (CollectErrors _ ae) =
    CollectErrors Nothing ae

liftPair :: (Monoid es) => (a -> (c,d)) -> (CollectErrors es a) -> (CollectErrors es c, CollectErrors es d)
liftPair f (CollectErrors (Just a) ae) = 
  (CollectErrors (Just c) ae, CollectErrors (Just d) ae)
  where
  (c,d) = f a
liftPair _ (CollectErrors _ ae) = 
  (CollectErrors Nothing ae, CollectErrors Nothing ae)

lift2 :: (Monoid es) => (a -> b -> c) -> (CollectErrors es a) -> (CollectErrors es b) -> (CollectErrors es c)
lift2 = liftA2

lift2CE :: (Monoid es) => (a -> b -> (CollectErrors es c)) -> (CollectErrors es a) -> (CollectErrors es b) -> (CollectErrors es c)
lift2CE f (CollectErrors (Just a) ae) (CollectErrors (Just b) be) =
  prependErrors (ae <> be) $ f a b
lift2CE _ (CollectErrors _ ae) (CollectErrors _ be) =
    CollectErrors Nothing (ae <> be)

lift1T :: (Monoid es) => (a -> b -> c) -> (CollectErrors es a) -> b -> (CollectErrors es c)
lift1T fn (CollectErrors (Just a) ae) b = CollectErrors (Just (fn a b)) ae
lift1T _ (CollectErrors _ ae) _ = CollectErrors Nothing ae

lift1TCE :: (Monoid es) => (a -> b -> (CollectErrors es c)) -> (CollectErrors es a) -> b -> (CollectErrors es c)
lift1TCE fn (CollectErrors (Just a) ae) b = prependErrors ae $ fn a b
lift1TCE _ (CollectErrors _ ae) _ = CollectErrors Nothing ae

liftT1 :: (Monoid es) => (a -> b -> c) -> a -> (CollectErrors es b) -> (CollectErrors es c)
liftT1 fn a (CollectErrors (Just b) be) = CollectErrors (Just (fn a b)) be
liftT1 _ _ (CollectErrors _ be) = CollectErrors Nothing be

liftT1CE :: (Monoid es) => (a -> b -> (CollectErrors es c)) -> a -> (CollectErrors es b) -> (CollectErrors es c)
liftT1CE fn a (CollectErrors (Just b) be) = prependErrors be $ fn a b
liftT1CE _ _ (CollectErrors _ be) = CollectErrors Nothing be

lift2pair :: (Monoid es) => (a -> b -> (c,d)) -> (CollectErrors es a) -> (CollectErrors es b) -> (CollectErrors es c, CollectErrors es d)
lift2pair f (CollectErrors (Just a) ae) (CollectErrors (Just b) be) = 
  (CollectErrors (Just c) abe, CollectErrors (Just d) abe)
  where
  (c,d) = f a b
  abe = ae <> be
lift2pair _ (CollectErrors _ ae) (CollectErrors _ be) = 
  (CollectErrors Nothing abe, CollectErrors Nothing abe)
  where
  abe = ae <> be

lift1Tpair :: (Monoid es) => (a -> b -> (c,d)) -> (CollectErrors es a) -> b -> (CollectErrors es c, CollectErrors es d)
lift1Tpair f (CollectErrors (Just a) ae) b = 
  (CollectErrors (Just c) ae, CollectErrors (Just d) ae)
  where
  (c,d) = f a b
lift1Tpair _ (CollectErrors _ ae) _ = 
  (CollectErrors Nothing ae, CollectErrors Nothing ae)

liftT1pair :: (Monoid es) => (a -> b -> (c,d)) -> a -> (CollectErrors es b) -> (CollectErrors es c, CollectErrors es d)
liftT1pair f a (CollectErrors (Just b) be) = 
  (CollectErrors (Just c) be, CollectErrors (Just d) be)
  where
  (c,d) = f a b
liftT1pair _ _ (CollectErrors _ be) = 
  (CollectErrors Nothing be, CollectErrors Nothing be)

liftTakeErrors :: (CanTakeErrors es t2) => (t1 -> t2) -> (CollectErrors es t1 -> t2)
liftTakeErrors f (CollectErrors (Just v) e) = 
  takeErrors e $ f v
liftTakeErrors _f (CollectErrors _ e) = 
  takeErrorsNoValue e

class CanTakeErrors es t where
  takeErrors :: es -> t -> t
  takeErrorsNoValue :: es -> t

instance (Monoid es) => CanTakeErrors es (CollectErrors es t) where
  takeErrors es1 (CollectErrors v es2) = CollectErrors v (es1 <> es2)
  takeErrorsNoValue es = CollectErrors Nothing es

instance (CanTakeErrors es t1, CanTakeErrors es t2) => CanTakeErrors es (t1,t2) where
  takeErrors es (v1,v2) = (takeErrors es v1, takeErrors es v2)
  takeErrorsNoValue es = (takeErrorsNoValue es, takeErrorsNoValue es)
  