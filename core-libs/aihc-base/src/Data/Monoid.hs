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
import GHC.Base (Applicative (..), Functor (..), Maybe (..), Monad (..), (.))
import GHC.Enum (Bounded (..))
import GHC.Internal.Classes (Ord (..))
import GHC.Internal.Foldable (Foldable (..))
import GHC.Internal.Traversable (Traversable (..))

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

instance Applicative First where
  pure value = First (Just value)
  First functions <*> First values = First (functions <*> values)

instance Monad First where
  First value >>= f = First (value >>= getFirst . f)

instance Foldable First where
  foldr f initial (First value) = foldr f initial value
  foldl f initial (First value) = foldl f initial value

instance Traversable First where
  traverse f (First value) = fmap First (traverse f value)

instance Applicative Last where
  pure value = Last (Just value)
  Last functions <*> Last values = Last (functions <*> values)

instance Monad Last where
  Last value >>= f = Last (value >>= getLast . f)

instance Foldable Last where
  foldr f initial (Last value) = foldr f initial value
  foldl f initial (Last value) = foldl f initial value

instance Traversable Last where
  traverse f (Last value) = fmap Last (traverse f value)

instance (Applicative f) => Applicative (Alt f) where
  pure value = Alt (pure value)
  Alt functions <*> Alt values = Alt (functions <*> values)

instance (Alternative f) => Alternative (Alt f) where
  empty = Alt empty
  Alt left <|> Alt right = Alt (left <|> right)

instance (Monad f) => Monad (Alt f) where
  Alt value >>= f = Alt (value >>= getAlt . f)

instance (Foldable f) => Foldable (Alt f) where
  foldMap f (Alt values) = foldMap f values
  foldr f initial (Alt values) = foldr f initial values
  foldl f initial (Alt values) = foldl f initial values
  toList (Alt values) = toList values
  null (Alt values) = null values
  length (Alt values) = length values

instance (Traversable f) => Traversable (Alt f) where
  traverse f (Alt values) = fmap Alt (traverse f values)
