module Data.Type.OpenWitness.TypeRep where

import Data.Kind
import Data.Type.OpenWitness
import Data.Type.Witness
import Prelude

data TypeRep :: forall (k :: Type). k -> Type where
    SimpleTypeRep :: forall (k :: Type) (a :: k). IOWitness a -> TypeRep a
    ApplyTypeRep :: forall (k1 :: Type) (k2 :: Type) (p :: k1 -> k2) (a :: k1). TypeRep p -> TypeRep a -> TypeRep (p a)

instance TestHetEquality TypeRep where
    testHetEquality (SimpleTypeRep wa) (SimpleTypeRep wb) = testHetEquality wa wb
    testHetEquality (ApplyTypeRep tfa ta) (ApplyTypeRep tfb tb) = do
        HRefl <- testHetEquality tfa tfb
        HRefl <- testHetEquality ta tb
        return HRefl
    testHetEquality _ _ = Nothing

instance TestEquality TypeRep where
    testEquality wa wb = fmap hetHomoEq $ testHetEquality wa wb
