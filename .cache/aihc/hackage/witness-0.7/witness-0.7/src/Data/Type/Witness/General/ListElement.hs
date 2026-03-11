module Data.Type.Witness.General.ListElement where

import Data.Type.Witness.General.Representative
import Data.Type.Witness.Specific.List.Element
import Data.Type.Witness.Specific.List.List
import Import

type ListElementWitness :: forall k. (k -> Type) -> Constraint
class Is (ListType Proxy) (WitnessTypeList w) => ListElementWitness (w :: k -> Type) where
    type WitnessTypeList w :: [k]
    toListElementWitness :: forall t. w t -> ListElementType (WitnessTypeList w) t
    fromListElementWitness :: forall t. ListElementType (WitnessTypeList w) t -> w t

instance Is (ListType Proxy) tt => ListElementWitness (ListElementType tt) where
    type WitnessTypeList (ListElementType tt) = tt
    toListElementWitness = id
    fromListElementWitness = id
