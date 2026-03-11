module Data.Type.OpenWitness.Instance where

import Data.Constraint
import Data.Type.Equality
import Data.Type.OpenWitness.TypeRep
import Data.Type.Witness
import Prelude

data Instance =
    forall (t :: Constraint). t => MkInstance (TypeRep t)

findInstance :: [Instance] -> TypeRep t -> Maybe (Dict t)
findInstance [] _ = Nothing
findInstance (MkInstance ti:ii) t =
    case testHetEquality ti t of
        Just HRefl -> Just Dict
        Nothing -> findInstance ii t
