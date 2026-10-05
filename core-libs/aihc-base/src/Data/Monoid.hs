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
    Ap (..),
  )
where

import Control.Applicative (Alternative (..), liftA2)
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
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Foldable (Foldable (..))
import GHC.Internal.Traversable (Traversable (..))
import GHC.Num (Num (..))
import GHC.Show (Show (..), showParen, showString, shows)

newtype First a = First {getFirst :: Maybe a}

newtype Last a = Last {getLast :: Maybe a}

newtype Alt f a = Alt {getAlt :: f a}

-- | This wrapper combines values in an applicative context.
newtype Ap f a = Ap {getAp :: f a}

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

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  Ap left <> Ap right = Ap (liftA2 (<>) left right)

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = Ap (pure mempty)

instance (Functor f) => Functor (Ap f) where
  fmap f (Ap values) = Ap (fmap f values)

instance (Applicative f) => Applicative (Ap f) where
  pure value = Ap (pure value)
  Ap functions <*> Ap values = Ap (functions <*> values)

instance (Alternative f) => Alternative (Ap f) where
  empty = Ap empty
  Ap left <|> Ap right = Ap (left <|> right)

instance (Monad f) => Monad (Ap f) where
  Ap value >>= f = Ap (value >>= getAp . f)

instance (Foldable f) => Foldable (Ap f) where
  foldMap f (Ap values) = foldMap f values
  foldr f initial (Ap values) = foldr f initial values
  foldl f initial (Ap values) = foldl f initial values

instance (Traversable f) => Traversable (Ap f) where
  traverse f (Ap values) = fmap Ap (traverse f values)

instance (Eq (f a)) => Eq (Ap f a) where
  Ap left == Ap right = left == right

instance (Ord (f a)) => Ord (Ap f a) where
  compare (Ap left) (Ap right) = compare left right

instance (Show (f a)) => Show (Ap f a) where
  showsPrec precedence (Ap values) =
    showParen (precedence >= 11) (showString "Ap {getAp = " . shows values . showString "}")

instance (Applicative f, Num a) => Num (Ap f a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger value = pure (fromInteger value)
