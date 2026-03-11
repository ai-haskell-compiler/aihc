-- | For a datatype where /every/ subterm is interesting, it is
-- possible to leverage 'Generic' to automatically produce the
-- 'GTraversable' instance.
--
-- This module defines a default 'GTraversable' instance for most
-- 'Generic' types, though you can override it with a custom instance
-- if you so wish. The 'gtraverse' implementation for this instance
-- traverses every subterm, and traverses left-to-right on products
-- ':*:'.
--
-- Example usage:
--
-- >{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
-- >import Data.Maybe (isJust)
-- >
-- >data MyConfig = MyConfig
-- >  { firstKey  :: Maybe Int
-- >  , secondKey :: Maybe String
-- >  , thirdKey  :: Maybe Bool
-- >  }
-- >  deriving (Generic)
-- >
-- >class SettableConfigKey a where
-- >  isSet :: a -> Bool
-- >
-- >instance SettableConfigKey (Maybe a) where
-- >  isSet = isJust
-- >
-- >isAnyConfigKeySet :: MyConfig -> Bool
-- >isAnyConfigKeySet = gfoldr @SettableConfigKey ((||) . isSet)
--
-- For 'Generic' types that have 'Rec1' in their representation, this
-- module will not work for GHC versions below 8.6.1, as the instance
-- makes use of @QuantifiedConstraints@. Instead, the instance for
-- 'Rec1' will cause a type error.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

# if MIN_VERSION_GLASGOW_HASKELL(8, 6, 1, 0)
{-# LANGUAGE QuantifiedConstraints #-}
# else
{-# LANGUAGE DataKinds #-}
# endif

{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Generics.Traversable.Generic where

import Data.Generics.Traversable
import GHC.Generics

# if MIN_VERSION_GLASGOW_HASKELL(8, 6, 1, 0)
# else
import GHC.TypeLits (TypeError, ErrorMessage (..))
# endif

-- | Special version of 'GTraversable' for the representation types
-- from 'Generic'.
class GTraversable' c (f :: * -> *) where
  gtraverse' :: Applicative g => (forall d. c d => d -> g d) -> (forall p. f p -> g (f p))

instance GTraversable' c U1 where
  gtraverse' _f U1 = pure U1

instance GTraversable' c V1 where
  gtraverse' _f = error "Impossible"

instance GTraversable' c Par1 where
  gtraverse' _f par1 = pure par1

# if MIN_VERSION_GLASGOW_HASKELL(8, 6, 1, 0)
instance (forall p. GTraversable c (f p)) => GTraversable' c (Rec1 f) where
  gtraverse' f (Rec1 recur) = Rec1 <$> gtraverse @c f recur
# else
instance
    ( TypeError
      ( 'Text "No generic instance can be derived for " ':$$: 'ShowType f ':$$:
        'Text " for this GHC version - upgrade to at least 8.6.1 to be" ':$$:
        'Text " able to use `GTraversable` from `Generic` here"
      )
    )
  => GTraversable' c (Rec1 f) where
  gtraverse' _ _ = error "Cannot implement `gtraverse'` for this type"
# endif

instance (c con) => GTraversable' c (K1 i con) where
  gtraverse' f (K1 con) = K1 <$> f con

instance (GTraversable' c f) => GTraversable' c (M1 i meta f) where
  gtraverse' f (M1 inner) = M1 <$> gtraverse' @c f inner

instance (GTraversable' c f, GTraversable' c g) => GTraversable' c (f :+: g) where
  gtraverse' f (L1 val) = L1 <$> gtraverse' @c f val
  gtraverse' f (R1 val) = R1 <$> gtraverse' @c f val

instance (GTraversable' c f, GTraversable' c g) => GTraversable' c (f :*: g) where
  gtraverse' f (left :*: right)
    = (:*:) <$> gtraverse' @c f left <*> gtraverse' @c f right

instance (Traversable f, GTraversable' c g) => GTraversable' c (f :.: g) where
  gtraverse' f (Comp1 comp) = Comp1 <$> traverse (gtraverse' @c f) comp

instance
    {-# OVERLAPPABLE #-}
    (Generic a, GTraversable' c (Rep a))
  => GTraversable c a
  where
  gtraverse f val
    = to <$> gtraverse' @c f (from val)
