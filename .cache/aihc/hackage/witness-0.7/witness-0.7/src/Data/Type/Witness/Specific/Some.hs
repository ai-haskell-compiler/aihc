module Data.Type.Witness.Specific.Some where

import Data.Type.Witness.General.AllConstraint
import Data.Type.Witness.General.Order
import Data.Type.Witness.General.WitnessConstraint
import Import

-- | Any value with a witness to a parameter of its type.
type SomeFor :: forall k. (k -> Type) -> (k -> Type) -> Type
type role SomeFor representational representational
data SomeFor f w =
    forall a. MkSomeFor (w a)
                        (f a)

mapSome ::
       forall k (g :: k -> Type) (w1 :: k -> Type) (w2 :: k -> Type).
       (forall t. w1 t -> w2 t)
    -> SomeFor g w1
    -> SomeFor g w2
mapSome f (MkSomeFor wt gt) = MkSomeFor (f wt) gt

matchSomeFor :: TestEquality w => w a -> SomeFor f w -> Maybe (f a)
matchSomeFor wit (MkSomeFor cwit cfa) = do
    Refl <- testEquality cwit wit
    return cfa

type SomeOf :: (Type -> Type) -> Type
type SomeOf = SomeFor Identity

pattern MkSomeOf :: w a -> a -> SomeOf w

pattern MkSomeOf wa a = MkSomeFor wa (Identity a)

{-# COMPLETE MkSomeOf #-}

instance (TestEquality w, WitnessConstraint Eq w) => Eq (SomeOf w) where
    MkSomeOf wa a == MkSomeOf wb b =
        case testEquality wa wb of
            Nothing -> False
            Just Refl ->
                case witnessConstraint @_ @Eq wa of
                    Dict -> a == b

matchSomeOf ::
       forall (w :: Type -> Type) (a :: Type). TestEquality w
    => w a
    -> SomeOf w
    -> Maybe a
matchSomeOf wit av = do
    Identity a <- matchSomeFor wit av
    return a

type Some :: forall k. (k -> Type) -> Type
type Some @k = SomeFor @k (Const ())

pattern MkSome :: w a -> Some w

pattern MkSome wa = MkSomeFor wa (Const ())

{-# COMPLETE MkSome #-}

matchSome ::
       forall k (w :: k -> Type) (a :: k). TestEquality w
    => w a
    -> Some w
    -> Bool
matchSome wit aw = isJust $ matchSomeFor wit aw

instance forall k (w :: k -> Type). TestEquality w => Eq (Some w) where
    (==) (MkSome wa) = matchSome wa

instance forall k (w :: k -> Type). TestOrder w => Ord (Some w) where
    compare (MkSome wa) (MkSome wb) = wOrderingToOrdering $ testCompare wa wb

withSomeAllConstraint ::
       forall k (c :: Type -> Constraint) (w :: k -> Type) (r :: Type). AllConstraint c w
    => Some w
    -> (forall a. c (w a) => w a -> r)
    -> r
withSomeAllConstraint (MkSome wa) call = withAllConstraint @k @c wa $ call wa

instance forall k (w :: k -> Type). AllConstraint Show w => Show (Some w) where
    show swa = withSomeAllConstraint @k @Show swa show

someForToSome :: SomeFor f w -> Some w
someForToSome (MkSomeFor wa _) = MkSome wa
