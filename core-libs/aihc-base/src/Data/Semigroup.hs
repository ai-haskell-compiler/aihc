module Data.Semigroup
  ( Semigroup (..),
    stimesMonoid,
    stimesIdempotent,
    stimesIdempotentMonoid,
    NonEmpty (..),
    Min (..),
    Max (..),
    First (..),
    Last (..),
    WrappedMonoid (..),
    Arg (..),
    ArgMin,
    ArgMax,
  )
where

import Data.Semigroup.Internal
  ( Semigroup (..),
    stimesIdempotent,
    stimesIdempotentMonoid,
    stimesMonoid,
  )
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import Prelude (Bool (..), Eq (..), Ord (..), Ordering (..))

newtype Min a = Min {getMin :: a}

newtype Max a = Max {getMax :: a}

newtype First a = First {getFirst :: a}

newtype Last a = Last {getLast :: a}

newtype WrappedMonoid m = WrapMonoid {unwrapMonoid :: m}

data Arg a b = Arg a b

type ArgMin a b = Min (Arg a b)

type ArgMax a b = Max (Arg a b)

instance (Ord a) => Semigroup (Min a) where
  Min left <> Min right = Min (min left right)

instance (Ord a) => Semigroup (Max a) where
  Max left <> Max right = Max (max left right)

instance Semigroup (First a) where
  left <> _ = left

instance Semigroup (Last a) where
  _ <> right = right

instance (Semigroup m) => Semigroup (WrappedMonoid m) where
  WrapMonoid left <> WrapMonoid right = WrapMonoid (left <> right)

instance (Eq a) => Eq (Arg a b) where
  Arg left _ == Arg right _ = left == right
  Arg left _ /= Arg right _ = left /= right

instance (Ord a) => Ord (Arg a b) where
  compare (Arg left _) (Arg right _) = compare left right
  left < right = compare left right == LT
  left <= right = compare left right /= GT
  left > right = compare left right == GT
  left >= right = compare left right /= LT
  min left@(Arg leftKey _) right@(Arg rightKey _) =
    case leftKey <= rightKey of
      True -> left
      False -> right
  max left@(Arg leftKey _) right@(Arg rightKey _) =
    case leftKey >= rightKey of
      True -> left
      False -> right
