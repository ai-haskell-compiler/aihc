-- | This is an approximate re-implementation of "Data.Typeable" using open witnesses.
module Data.Type.OpenWitness.Typeable where

import Data.Kind
import Data.Type.OpenWitness
import Data.Type.OpenWitness.TypeRep
import Data.Type.Witness
import Prelude

-- | types of kind @Type@ with a representation
class Typeable (a :: k) where
    typeRep :: TypeRep a

instance (Typeable (f :: k1 -> k2), Typeable (a :: k1)) => Typeable (f a) where
    typeRep = ApplyTypeRep typeRep typeRep

instance Typeable Type where
    typeRep = SimpleTypeRep $(iowitness [t|Type|])

type Fun = (->)

instance Typeable (->) where
    typeRep = SimpleTypeRep $(iowitness [t|Fun|])

instance Typeable Constraint where
    typeRep = SimpleTypeRep $(iowitness [t|Constraint|])

instance Typeable TypeRep where
    typeRep = SimpleTypeRep $(iowitness [t|TypeRep|])

instance Typeable Typeable where
    typeRep = SimpleTypeRep $(iowitness [t|Typeable|])

instance Typeable () where
    typeRep = SimpleTypeRep $(iowitness [t|()|])

instance Typeable (,) where
    typeRep = SimpleTypeRep $(iowitness [t|(,)|])

instance Typeable Either where
    typeRep = SimpleTypeRep $(iowitness [t|Either|])

instance Typeable Maybe where
    typeRep = SimpleTypeRep $(iowitness [t|Maybe|])

instance Typeable [] where
    typeRep = SimpleTypeRep $(iowitness [t|[]|])

instance Typeable Bool where
    typeRep = SimpleTypeRep $(iowitness [t|Bool|])

instance Typeable Char where
    typeRep = SimpleTypeRep $(iowitness [t|Char|])

instance Typeable Int where
    typeRep = SimpleTypeRep $(iowitness [t|Int|])

cast ::
       forall (a :: Type) (b :: Type). (Typeable a, Typeable b)
    => a
    -> Maybe b
cast a = do
    Refl :: a :~: b <- testEquality typeRep typeRep
    return a

gcast ::
       forall (k :: Type) (a :: k) (b :: k) (c :: k -> Type). (Typeable a, Typeable b)
    => c a
    -> Maybe (c b)
gcast ca = do
    Refl :: a :~: b <- testEquality typeRep typeRep
    return ca

-- | given representations of @a@ and @b@, make a representation of @a -> b@
mkFunTy :: TypeRep a -> TypeRep b -> TypeRep (a -> b)
mkFunTy ta tb = ApplyTypeRep (ApplyTypeRep (typeRep :: TypeRep (->)) ta) tb

-- | given representations of @a -> b@ and @a@, make a representation of @b@ (otherwise not)
funResultTy :: TypeRep (a -> b) -> TypeRep a -> Maybe (TypeRep b)
funResultTy (ApplyTypeRep (ApplyTypeRep repFn' ta') tb') ta = do
    Refl <- testEquality repFn' (typeRep :: TypeRep (->))
    Refl <- testEquality ta' ta
    return tb'
funResultTy _ _ = Nothing

mkAppTy :: forall (k1 :: Type) (k2 :: Type) (f :: k1 -> k2) (a :: k1). TypeRep f -> TypeRep a -> TypeRep (f a)
mkAppTy = ApplyTypeRep
