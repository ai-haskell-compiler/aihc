{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Monoid
  ( Monoid (..),
    (<>),
    Dual (..),
    Endo (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    First (..),
    Last (..),
    Alt (..),
  )
where

import Control.Applicative (Alternative (..))
import Data.Semigroup
  ( Max (..),
    Min (..),
    WrappedMonoid (..),
  )
import Data.Semigroup.Internal
  ( All (..),
    Any (..),
    Dual (..),
    Endo (..),
    Monoid (..),
    Product (..),
    Semigroup (..),
    Sum (..),
  )
import GHC.Base (Functor (..), Maybe (..))
import GHC.Enum (Bounded (..))
import GHC.Internal.Classes (Ord (..))

newtype First a = First {getFirst :: Maybe a}

newtype Last a = Last {getLast :: Maybe a}

newtype Alt f a = Alt {getAlt :: f a}

instance Semigroup (First a) where
  First Nothing <> right = right
  left <> _ = left

instance Monoid (First a) where
  mempty = First Nothing

instance Semigroup (Last a) where
  left <> Last Nothing = left
  _ <> right = right

instance Monoid (Last a) where
  mempty = Last Nothing

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

deriving newtype instance (Monoid m) => Monoid (WrappedMonoid m)

instance (Alternative f) => Semigroup (Alt f a) where
  Alt left <> Alt right = Alt (left <|> right)

instance (Alternative f) => Monoid (Alt f a) where
  mempty = Alt empty

instance Functor First where
  fmap f (First value) = First (fmap f value)

instance Functor Last where
  fmap f (Last value) = Last (fmap f value)

instance (Functor f) => Functor (Alt f) where
  fmap f (Alt values) = Alt (fmap f values)
