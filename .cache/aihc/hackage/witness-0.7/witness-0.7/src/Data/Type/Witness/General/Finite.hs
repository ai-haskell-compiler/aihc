{-# OPTIONS -fno-warn-orphans #-}

module Data.Type.Witness.General.Finite where

import Data.Type.Witness.General.AllConstraint
import Data.Type.Witness.General.WitnessConstraint
import Data.Type.Witness.Specific.All
import Data.Type.Witness.Specific.Some
import Import

type FiniteWitness :: forall k. (k -> Type) -> Constraint
class FiniteWitness (w :: k -> Type) where
    assembleAllFor ::
           forall (m :: Type -> Type) (f :: k -> Type). Applicative m
        => (forall (t :: k). w t -> m (f t))
        -> m (AllFor f w)

instance FiniteWitness ((:~:) t) where
    assembleAllFor getsel = fmap (\ft -> MkAllFor $ \Refl -> ft) $ getsel Refl

instance (TestEquality w, FiniteWitness w) => Countable (Some w) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance (TestEquality w, FiniteWitness w) => Searchable (Some w) where
    search = finiteSearch

instance (TestEquality w, FiniteWitness w) => Finite (Some w) where
    assemble ::
           forall b f. Applicative f
        => (Some w -> f b)
        -> f (Some w -> b)
    assemble afb =
        fmap (\(MkAllFor wtcb) (MkSome wt) -> getConst $ wtcb wt) $ assembleAllFor $ \wt -> fmap Const $ afb $ MkSome wt
    allValues = getConst $ assembleAllFor $ \wt -> Const [MkSome wt]

allWitnesses :: FiniteWitness w => [Some w]
allWitnesses = getConst $ assembleAllFor $ \wt -> Const [MkSome wt]

allForCodomain :: FiniteWitness w => AllFor f w -> [Some f]
allForCodomain af = fmap (allMapSome af) allWitnesses

instance (FiniteWitness w, AllConstraint Show w, WitnessConstraint Show w) => Show (AllOf w) where
    show (MkAllOf wtt) = let
        showItem :: Some w -> String
        showItem (MkSome wt) =
            allShow wt ++
            " -> " ++
            case witnessConstraint @_ @Show wt of
                Dict -> show (wtt wt)
        in "{" ++ intercalate "," (fmap showItem allWitnesses) ++ "}"

assembleAllOf :: (FiniteWitness w, Applicative m) => (forall t. w t -> m t) -> m (AllOf w)
assembleAllOf wtmt = fmap allForToAllOf $ assembleAllFor $ \wt -> fmap Identity $ wtmt wt
