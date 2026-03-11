module Data.Type.Witness.Specific.Empty where

import Data.Type.Witness.General.Finite
import Data.Type.Witness.General.ListElement
import Data.Type.Witness.General.Order
import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessConstraint
import Data.Type.Witness.Specific.All
import Import

type EmptyType :: forall k. k -> Type
type role EmptyType phantom
newtype EmptyType t =
    MkEmptyType Void
    deriving newtype (Eq, Countable, Searchable, Empty)

instance Finite (EmptyType t) where
    allValues = []
    assemble _ = pure never

instance TestEquality EmptyType where
    testEquality = never

instance TestOrder EmptyType where
    testCompare = never

instance Representative EmptyType where
    getRepWitness = never

instance FiniteWitness EmptyType where
    assembleAllFor _ = pure emptyAllFor

instance WitnessConstraint c EmptyType where
    witnessConstraint = never

instance ListElementWitness EmptyType where
    type WitnessTypeList EmptyType = '[]
    toListElementWitness wit = never wit
    fromListElementWitness lt = never lt

emptyAllOf :: AllOf EmptyType
emptyAllOf = MkAllOf never

emptyAllFor :: AllFor f EmptyType
emptyAllFor = MkAllFor never
