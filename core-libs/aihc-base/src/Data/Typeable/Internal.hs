{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Typeable.Internal
  ( Typeable,
    TypeRep,
    TyCon,
    Proxy (..),
    (:~:) (..),
    (:~~:) (..),
    cast,
    eqT,
    heqT,
    gcast,
    gcast1,
    gcast2,
    typeOf,
    typeOf1,
    typeOf2,
    typeOf3,
    typeOf4,
    typeOf5,
    typeOf6,
    typeOf7,
    typeRep,
    showsTypeRep,
    typeRepArgs,
    typeRepTyCon,
    splitTyConApp,
    tyConPackage,
    tyConModule,
    tyConName,
    rnfTyCon,
    rnfTypeRep,
  )
where

import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..), (:~~:) (..))
import GHC.Base (Maybe (..))
import GHC.Internal.Classes (Eq (..))
import GHC.Prim.Show (Show (..), ShowS)
import GHC.Types (Bool (..), Int (..), Type)
import Type.Reflection (SomeTypeRep (..), TyCon, Typeable, rnfSomeTypeRep, rnfTyCon, splitApps, tyConModule, tyConName, tyConPackage)
import Type.Reflection qualified as Reflection
import Unsafe.Coerce (unsafeCoerce)

type TypeRep = SomeTypeRep

typeRep :: forall k proxy (a :: k). (Typeable a) => proxy a -> TypeRep
typeRep = Reflection.someTypeRep

showsTypeRep :: TypeRep -> ShowS
showsTypeRep = showsPrec (I# 0#)

typeOf :: (Typeable a) => a -> TypeRep
typeOf value = SomeTypeRep (Reflection.typeOf value)

-- | The representation of the type constructor of a value, applied to all
-- arguments but the last ones.
typeOf1 :: forall t (a :: Type). (Typeable t) => t a -> TypeRep
typeOf1 _ = typeRep (Proxy :: Proxy t)

typeOf2 :: forall t (a :: Type) (b :: Type). (Typeable t) => t a b -> TypeRep
typeOf2 _ = typeRep (Proxy :: Proxy t)

typeOf3 :: forall t (a :: Type) (b :: Type) (c :: Type). (Typeable t) => t a b c -> TypeRep
typeOf3 _ = typeRep (Proxy :: Proxy t)

typeOf4 :: forall t (a :: Type) (b :: Type) (c :: Type) (d :: Type). (Typeable t) => t a b c d -> TypeRep
typeOf4 _ = typeRep (Proxy :: Proxy t)

typeOf5 :: forall t (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type). (Typeable t) => t a b c d e -> TypeRep
typeOf5 _ = typeRep (Proxy :: Proxy t)

typeOf6 :: forall t (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type) (f :: Type). (Typeable t) => t a b c d e f -> TypeRep
typeOf6 _ = typeRep (Proxy :: Proxy t)

typeOf7 :: forall t (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type) (f :: Type) (g :: Type). (Typeable t) => t a b c d e f g -> TypeRep
typeOf7 _ = typeRep (Proxy :: Proxy t)

typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon (SomeTypeRep rep) = Reflection.typeRepTyCon rep

eqTypeRep :: TypeRep -> TypeRep -> Bool
eqTypeRep = (==)

rnfTypeRep :: TypeRep -> ()
rnfTypeRep = rnfSomeTypeRep

-- | The arguments of a type.
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (SomeTypeRep rep) =
  case splitApps rep of
    (_, arguments) -> arguments

-- | The type constructor and the arguments of a type.
splitTyConApp :: TypeRep -> (TyCon, [TypeRep])
splitTyConApp (SomeTypeRep rep) = splitApps rep

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast value = castWith value Proxy

castWith :: (Typeable a, Typeable b) => a -> Proxy b -> Maybe b
castWith value target =
  if eqTypeRep (typeOf value) (typeRep target)
    then Just (unsafeCoerce value)
    else Nothing

-- | Extract a witness of equality of two types.
eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
eqT =
  if eqTypeRep (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))
    then Just (unsafeCoerce (Refl :: a :~: a))
    else Nothing

-- | Extract a witness of heterogeneous equality of two types.
heqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~~: b)
heqT =
  if eqTypeRep (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))
    then Just (unsafeCoerce (HRefl :: a :~~: a))
    else Nothing

-- | Cast a value under a type constructor.
gcast :: forall {k} (a :: k) (b :: k) (c :: k -> Type). (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast value = gcastWith value Proxy Proxy

gcastWith :: forall k (c :: k -> Type) (a :: k) (b :: k). (Typeable a, Typeable b) => c a -> Proxy a -> Proxy b -> Maybe (c b)
gcastWith value source target =
  if eqTypeRep (typeRep source) (typeRep target)
    then Just (unsafeCoerce value)
    else Nothing

-- | Cast a value over a unary type constructor.
gcast1 :: forall (c :: Type -> Type) (t :: Type -> Type) (t' :: Type -> Type) a. (Typeable t, Typeable t') => c (t a) -> Maybe (c (t' a))
gcast1 value = gcast1With value Proxy Proxy

gcast1With :: forall (c :: Type -> Type) (t :: Type -> Type) (t' :: Type -> Type) a. (Typeable t, Typeable t') => c (t a) -> Proxy t -> Proxy t' -> Maybe (c (t' a))
gcast1With value source target =
  if eqTypeRep (typeRep source) (typeRep target)
    then Just (unsafeCoerce value)
    else Nothing

-- | Cast a value over a binary type constructor.
gcast2 :: forall (c :: Type -> Type) (t :: Type -> Type -> Type) (t' :: Type -> Type -> Type) a b. (Typeable t, Typeable t') => c (t a b) -> Maybe (c (t' a b))
gcast2 value = gcast2With value Proxy Proxy

gcast2With :: forall (c :: Type -> Type) (t :: Type -> Type -> Type) (t' :: Type -> Type -> Type) a b. (Typeable t, Typeable t') => c (t a b) -> Proxy t -> Proxy t' -> Maybe (c (t' a b))
gcast2With value source target =
  if eqTypeRep (typeRep source) (typeRep target)
    then Just (unsafeCoerce value)
    else Nothing
