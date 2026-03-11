module Data.Type.Witness.General.WitnessValue where

import Data.Type.Witness.Specific.Some
import Import

type WitnessValue :: forall k. (k -> Type) -> Constraint
class WitnessValue w where
    type WitnessValueType w :: Type
    witnessToValue :: forall t. w t -> WitnessValueType w
    valueToWitness :: forall r. WitnessValueType w -> (forall t. w t -> r) -> r

someToValue ::
       forall k (w :: k -> Type). WitnessValue w
    => Some w
    -> WitnessValueType w
someToValue (MkSome wt) = witnessToValue wt

valueToSome ::
       forall k (w :: k -> Type). WitnessValue w
    => WitnessValueType w
    -> Some w
valueToSome v = valueToWitness v MkSome
