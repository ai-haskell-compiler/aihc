-- | This is an approximate re-implementation of "Data.Dynamic" using open witnesses.
module Data.Type.OpenWitness.Dynamic where

import Data.Kind
import Data.Type.OpenWitness.TypeRep
import Data.Type.OpenWitness.Typeable
import Data.Type.Witness
import Prelude

-- * The @Dynamic@ type
type Dynamic = SomeOf TypeRep

-- * Converting to and from @Dynamic@
toDyn ::
       forall (a :: Type). Typeable a
    => a
    -> Dynamic
toDyn = MkSomeOf typeRep

fromDyn :: Typeable a => Dynamic -> a -> a
fromDyn dyn def =
    case fromDynamic dyn of
        Just a -> a
        _ -> def

fromDynamic ::
       forall (a :: Type). Typeable a
    => Dynamic
    -> Maybe a
fromDynamic (MkSomeOf uq a) = do
    Refl <- testEquality uq (typeRep :: TypeRep a)
    return a

-- * Applying functions of dynamic type
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (MkSomeOf (ApplyTypeRep (ApplyTypeRep repFn' rx') ry) f) (MkSomeOf rx x) = do
    HRefl <- testHetEquality repFn' (typeRep :: TypeRep (->))
    HRefl <- testHetEquality rx' rx
    return (MkSomeOf ry (f x))
dynApply _ _ = Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp a b =
    case (dynApply a b) of
        Just d -> d
        _ -> error "Type error in dynamic application.\nCan't apply function to argument"

dynTypeRep :: Dynamic -> Some (TypeRep :: Type -> Type)
dynTypeRep (MkSomeOf r _) = MkSome r
