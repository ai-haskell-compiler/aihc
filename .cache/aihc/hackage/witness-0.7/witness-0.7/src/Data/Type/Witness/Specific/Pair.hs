module Data.Type.Witness.Specific.Pair where

import Data.Type.Witness.General.Order
import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessConstraint
import Import

-- | a witness for pairs of witnesses
type PairType :: (k -> Type) -> (k -> Type) -> (k -> Type)
type role PairType representational representational nominal
data PairType w1 w2 t =
    MkPairType (w1 t)
               (w2 t)

instance (Representative w1, Representative w2) => Representative (PairType w1 w2) where
    getRepWitness (MkPairType w1 w2) =
        case (getRepWitness w1, getRepWitness w2) of
            (Dict, Dict) -> Dict

instance (Is w1 t, Is w2 t) => Is (PairType w1 w2) t where
    representative = MkPairType representative representative

-- | left-biased
instance TestEquality w1 => TestEquality (PairType w1 w2) where
    testEquality (MkPairType a1 _) (MkPairType b1 _) = testEquality a1 b1

-- | left-biased
instance TestOrder w1 => TestOrder (PairType w1 w2) where
    testCompare (MkPairType a1 _) (MkPairType b1 _) = testCompare a1 b1

-- | right-biased
instance WitnessConstraint c w2 => WitnessConstraint c (PairType w1 w2) where
    witnessConstraint (MkPairType _ w2) = witnessConstraint w2
