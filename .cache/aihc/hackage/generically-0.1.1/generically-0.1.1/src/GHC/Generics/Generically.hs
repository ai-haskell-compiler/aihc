{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
-- | This module exports 'Generically' and 'Generically' newtypes
-- meant to be used with "GHC.Generics" and @DerivingVia@.
--
-- These types are re-exported from "GHC.Generics" on @base-4.17@ and later,
-- and defined here for older @base@ versions.
--
module GHC.Generics.Generically (
    Generically (..),
    Generically1 (..),
) where

#if MIN_VERSION_base(4,17,0)
import GHC.Generics
#if !MIN_VERSION_base(4,18,0)
import Data.Orphans () -- To bring Eq/Ord instances for Generically1 into scope
#endif
#else

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

import GHC.Generics
import Control.Applicative (liftA2)
import Control.Applicative (Alternative (..))
import Data.Functor.Classes (Ord1 (..), Eq1 (..))

-------------------------------------------------------------------------------
-- Generically
-------------------------------------------------------------------------------

-- | A type whose instances are defined generically, using the
-- 'Generic' representation.
newtype Generically a = Generically a

instance (Generic a, Semigroup (Rep a ())) => Semigroup (Generically a) where
  (<>) :: Generically a -> Generically a -> Generically a
  Generically a <> Generically b = Generically (to (from a <> from b :: Rep a ()))

instance (Generic a, Monoid (Rep a ())) => Monoid (Generically a) where
  mempty :: Generically a
  mempty = Generically (to (mempty :: Rep a ()))

  mappend :: Generically a -> Generically a -> Generically a
#if MIN_VERSION_base(4,11,0)
  mappend = (<>)
#else
  mappend (Generically a) (Generically b) = Generically (to (mappend (from a) (from b) :: Rep a ()))
#endif

-------------------------------------------------------------------------------
-- Generically1
-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 810
type    Generically1 :: forall k. (k -> Type) -> (k -> Type)
#endif

-- | A datatype whose instances are defined generically, using the
-- 'Generic' representation. 'Generically1' is a higher-kinded version
-- of 'Generically' that uses 'Generic1'.
newtype Generically1 f a = Generically1 (f a)

instance (Generic1 f, Functor (Rep1 f)) => Functor (Generically1 f) where
  fmap :: (a -> a') -> (Generically1 f a -> Generically1 f a')
  fmap f (Generically1 as) = Generically1 (to1 (fmap f (from1 as)))

  (<$) :: a -> Generically1 f b -> Generically1 f a
  a <$ Generically1 as = Generically1 (to1 (a <$ from1 as))

instance (Generic1 f, Applicative (Rep1 f)) => Applicative (Generically1 f) where
  pure :: a -> Generically1 f a
  pure a = Generically1 (to1 (pure a))

  (<*>) :: Generically1 f (a -> b) -> Generically1 f a -> Generically1 f b
  Generically1 fs <*> Generically1 as = Generically1 (to1 (from1 fs <*> from1 as))

#if MIN_VERSION_base(4,10,0)
  liftA2 :: (a -> b -> c) -> (Generically1 f a -> Generically1 f b -> Generically1 f c)
  liftA2 f (Generically1 as) (Generically1 bs) = Generically1 (to1 (liftA2 f (from1 as) (from1 bs)))
#endif

instance (Generic1 f, Alternative (Rep1 f)) => Alternative (Generically1 f) where
  empty :: Generically1 f a
  empty = Generically1 (to1 empty)

  (<|>) :: Generically1 f a -> Generically1 f a -> Generically1 f a
  Generically1 as1 <|> Generically1 as2 = Generically1 (to1 (from1 as1 <|> from1 as2))

instance (Generic1 f, Eq (Rep1 f a)) => Eq (Generically1 f a) where
   Generically1 x == Generically1 y = from1 x == from1 y
   Generically1 x /= Generically1 y = from1 x /= from1 y

instance (Generic1 f, Ord (Rep1 f a)) => Ord (Generically1 f a) where
   Generically1 x `compare` Generically1 y = from1 x `compare` from1 y

instance (Generic1 f, Eq1 (Rep1 f)) => Eq1 (Generically1 f) where
  liftEq :: (a -> b -> Bool) -> (Generically1 f a -> Generically1 f b -> Bool)
  liftEq eq (Generically1 as1) (Generically1 as2) = liftEq eq (from1 as1) (from1 as2)

instance (Generic1 f, Ord1 (Rep1 f)) => Ord1 (Generically1 f) where
  liftCompare :: (a -> b -> Ordering) -> (Generically1 f a -> Generically1 f b -> Ordering)
  liftCompare cmp (Generically1 as1) (Generically1 as2) = liftCompare cmp (from1 as1) (from1 as2)

-- MIN_VERSION_base(4,17,0)
#endif
