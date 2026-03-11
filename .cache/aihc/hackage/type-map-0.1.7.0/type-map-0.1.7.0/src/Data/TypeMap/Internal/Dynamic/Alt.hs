{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TypeMap.Internal.Dynamic.Alt where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>))
#endif
import Data.Typeable
#if MIN_VERSION_base(4,10,0)
import GHC.Exts (Any)
import qualified Type.Reflection as T
#else
import GHC.Prim (Any, Proxy#)
#endif
import Unsafe.Coerce
import qualified Data.Map as Map

import Data.TypeMap.Internal.Dynamic
  (TypeMap(..), Item, Typed, UnTyped, ItemFun, ItemKleisli)

-- | Insert an element indexed by type @t@.
insert
  :: forall t x
  .  Typeable t => Item x t -> TypeMap x -> TypeMap x
insert v (TypeMap m) = TypeMap (Map.insert (typeRep (Proxy @t)) (coerce v) m)
  where
    coerce :: Item x t -> Any
    coerce = unsafeCoerce

-- What is a good fixity for (<:)?

-- | Infix version of 'insert' to facilitate a literal-ish syntax:
--
-- @
-- 'Data.TypeMap.Dynamic.empty'
--   '<:' 'at' @t1 v1
--   '<:' 'at' @t2 v2
-- @
(<:)
  :: forall t x proxy
  .  Typeable t => TypeMap x -> (proxy t, Item x t) -> TypeMap x
(<:) tm (_, v) = insert @t v tm

-- | See @('<:')@.
at :: forall t a
   .  Typeable t => a -> (Proxy t, a)
at = (,) Proxy

-- | Update an element indexed by type @t@.
update
  :: forall t x
  .  Typeable t => (Item x t -> Maybe (Item x t)) -> TypeMap x -> TypeMap x
update f (TypeMap m) = TypeMap (Map.update (coerce f) (typeRep (Proxy @t)) m)
  where
    coerce :: (Item x t -> Maybe (Item x t)) -> (Any -> Maybe Any)
    coerce = unsafeCoerce

-- | Update a (possibly absent) element indexed by type @t@.
alter
  :: forall t x
  .  Typeable t => (Maybe (Item x t) -> Maybe (Item x t)) -> TypeMap x -> TypeMap x
alter f (TypeMap m) = TypeMap (Map.alter (coerce f) (typeRep (Proxy @t)) m)
  where
    coerce :: (Maybe (Item x t) -> Maybe (Item x t)) -> (Maybe Any -> Maybe Any)
    coerce = unsafeCoerce

-- | Lookup an element indexed by type @t@.
lookup
  :: forall t x
  .  Typeable t => TypeMap x -> Maybe (Item x t)
lookup (TypeMap m) = coerce (Map.lookup (typeRep (Proxy @t)) m)
  where
    coerce :: Maybe Any -> Maybe (Item x t)
    coerce = unsafeCoerce

-- | Delete a key and its value from the map.
-- Does nothing if the key does not exist.
delete
  :: forall t x
  .  Typeable t => TypeMap x -> TypeMap x
delete (TypeMap m) = TypeMap (Map.delete (typeRep (Proxy @t)) m)

-- | Map a function on all elements.
map
  :: forall x y. (forall t. Typeable t => Item x t -> Item y t)
  -> TypeMap x -> TypeMap y
map f (TypeMap m) = TypeMap (Map.mapWithKey f' m)
  where
    f' = withTypeRep @(ItemFun x y)
      (Typed_ (f @t) :: forall t. Typeable t => Typed_ (ItemFun x y) t)

-- | Traverse the type map. ('map' with effects.)
traverse
  :: forall f x y
  .  Applicative f
  => (forall t. Typeable t => Item x t -> f (Item y t))
  -> TypeMap x -> f (TypeMap y)
traverse f (TypeMap m) = TypeMap <$> Map.traverseWithKey f' m
  where
    f' = withTypeRep @(ItemKleisli f x y)
      (Typed_ (f @t) :: forall t. Typeable t => Typed_ (ItemKleisli f x y) t)

-- * Unsafe internals

newtype Typed_ x t = Typed_ (Typed x t)

withTypeRep
  :: forall x
  .  (forall t. Typeable t => Typed_ x t)
  -> TypeRep -> UnTyped x
#if MIN_VERSION_base(4,10,0)
withTypeRep f someRep =
  case someRep of
    T.SomeTypeRep (rep' :: T.TypeRep t) ->
      -- We still need to unsafely coerce the kind of t to Type
      -- and Typed to UnTyped
      (unsafeCoerce
        ((\rep -> T.withTypeable rep f) :: forall a. T.TypeRep a -> Typed_ x a)
        :: T.TypeRep t -> UnTyped x) rep'
#else
withTypeRep f rep =
  withTypeable (WithTypeable f :: WithTypeable x) (\_ -> rep)

newtype WithTypeable x
  = WithTypeable (forall t. Typeable t => Typed_ x t)

withTypeable
  :: WithTypeable x -> (Proxy# () -> TypeRep) -> UnTyped x
withTypeable = unsafeCoerce
#endif
