module Data.Monoid
  ( Monoid (..),
    (<>),
    Dual (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    First (..),
    Last (..),
  )
where

import Data.Semigroup
  ( Max (..),
    Min (..),
    Semigroup (..),
    WrappedMonoid (..),
  )
import Prelude
  ( Bool (..),
    Bounded (..),
    Maybe (..),
    Num (..),
    (&&),
    (||),
  )

class (Semigroup a) => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

  mappend = (<>)
  mconcat = foldMonoid

infixr 6 `mappend`

newtype Dual a = Dual {getDual :: a}

newtype All = All {getAll :: Bool}

newtype Any = Any {getAny :: Bool}

newtype Sum a = Sum {getSum :: a}

newtype Product a = Product {getProduct :: a}

newtype First a = First {getFirst :: Maybe a}

newtype Last a = Last {getLast :: Maybe a}

instance (Semigroup a) => Semigroup (Dual a) where
  Dual left <> Dual right = Dual (right <> left)

instance (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty

instance Semigroup All where
  All left <> All right = All (left && right)

instance Monoid All where
  mempty = All True

instance Semigroup Any where
  Any left <> Any right = Any (left || right)

instance Monoid Any where
  mempty = Any False

instance (Num a) => Semigroup (Sum a) where
  Sum left <> Sum right = Sum (left + right)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

instance (Num a) => Semigroup (Product a) where
  Product left <> Product right = Product (left * right)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

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

instance (Monoid m) => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty

{- HLINT ignore foldMonoid "Use foldr" -}
foldMonoid :: (Monoid a) => [a] -> a
foldMonoid [] = mempty
foldMonoid (value : values) = value <> foldMonoid values

instance Monoid [a] where
  mempty = []

instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing

instance Monoid () where
  mempty = ()

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
