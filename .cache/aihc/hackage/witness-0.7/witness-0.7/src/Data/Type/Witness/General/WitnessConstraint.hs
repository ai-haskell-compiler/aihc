module Data.Type.Witness.General.WitnessConstraint where

import Import

type WitnessConstraint :: forall k. (k -> Constraint) -> (k -> Type) -> Constraint
class WitnessConstraint c w where
    witnessConstraint :: forall t. w t -> Dict (c t)

instance WitnessConstraint c (Compose Dict c) where
    witnessConstraint (Compose d) = d

instance c t => WitnessConstraint c ((:~:) t) where
    witnessConstraint Refl = Dict
