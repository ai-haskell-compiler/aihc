module Data.Type.Witness.General.HetConstraint where

import Import

type HetConstraintWitness :: forall k1. (k1 -> Constraint) -> forall k2. k2 -> Type
type role HetConstraintWitness representational nominal
data HetConstraintWitness c t where
    MkHetConstraintWitness
        :: forall k (c :: k -> Constraint) (t :: k). c t
        => HetConstraintWitness c t

type HetConstraint :: forall k1. (k1 -> Constraint) -> forall k2. k2 -> Constraint
class HetConstraint c t where
    hetConstraint :: HetConstraintWitness c t

instance forall k (c :: k -> Constraint) (t :: k). c t => HetConstraint c t where
    hetConstraint = MkHetConstraintWitness
