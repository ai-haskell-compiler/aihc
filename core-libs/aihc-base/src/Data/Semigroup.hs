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
    Dual (..),
    Endo (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    Arg (..),
    ArgMin,
    ArgMax,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Semigroup.Internal
  ( All (..),
    Any (..),
    Dual (..),
    Endo (..),
    Product (..),
    Semigroup (..),
    Sum (..),
    stimesIdempotent,
    stimesIdempotentMonoid,
    stimesMonoid,
  )
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import Prelude (Applicative (..), Bool (..), Eq (..), Foldable (..), Functor (..), Monad (..), Ord (..), Ordering (..), Traversable (..))

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

-- | Each of these wrappers only changes how values combine, so its 'Eq' and
-- 'Ord' are those of the value it wraps.
instance (Eq a) => Eq (Min a) where
  Min left == Min right = left == right
  Min left /= Min right = left /= right

instance (Ord a) => Ord (Min a) where
  compare (Min left) (Min right) = compare left right
  Min left < Min right = left < right
  Min left <= Min right = left <= right
  Min left > Min right = left > right
  Min left >= Min right = left >= right
  max (Min left) (Min right) = Min (max left right)
  min (Min left) (Min right) = Min (min left right)

instance (Eq a) => Eq (Max a) where
  Max left == Max right = left == right
  Max left /= Max right = left /= right

instance (Ord a) => Ord (Max a) where
  compare (Max left) (Max right) = compare left right
  Max left < Max right = left < right
  Max left <= Max right = left <= right
  Max left > Max right = left > right
  Max left >= Max right = left >= right
  max (Max left) (Max right) = Max (max left right)
  min (Max left) (Max right) = Max (min left right)

instance (Eq a) => Eq (First a) where
  First left == First right = left == right
  First left /= First right = left /= right

instance (Ord a) => Ord (First a) where
  compare (First left) (First right) = compare left right
  First left < First right = left < right
  First left <= First right = left <= right
  First left > First right = left > right
  First left >= First right = left >= right
  max (First left) (First right) = First (max left right)
  min (First left) (First right) = First (min left right)

instance (Eq a) => Eq (Last a) where
  Last left == Last right = left == right
  Last left /= Last right = left /= right

instance (Ord a) => Ord (Last a) where
  compare (Last left) (Last right) = compare left right
  Last left < Last right = left < right
  Last left <= Last right = left <= right
  Last left > Last right = left > right
  Last left >= Last right = left >= right
  max (Last left) (Last right) = Last (max left right)
  min (Last left) (Last right) = Last (min left right)

instance (Eq m) => Eq (WrappedMonoid m) where
  WrapMonoid left == WrapMonoid right = left == right
  WrapMonoid left /= WrapMonoid right = left /= right

instance (Ord m) => Ord (WrappedMonoid m) where
  compare (WrapMonoid left) (WrapMonoid right) = compare left right
  WrapMonoid left < WrapMonoid right = left < right
  WrapMonoid left <= WrapMonoid right = left <= right
  WrapMonoid left > WrapMonoid right = left > right
  WrapMonoid left >= WrapMonoid right = left >= right
  max (WrapMonoid left) (WrapMonoid right) = WrapMonoid (max left right)
  min (WrapMonoid left) (WrapMonoid right) = WrapMonoid (min left right)

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

instance Functor Min where
  fmap f (Min value) = Min (f value)

instance Functor Max where
  fmap f (Max value) = Max (f value)

instance Functor First where
  fmap f (First value) = First (f value)

instance Functor Last where
  fmap f (Last value) = Last (f value)

instance Functor (Arg a) where
  fmap f (Arg key value) = Arg key (f value)

instance Bifunctor Arg where
  bimap f g (Arg key value) = Arg (f key) (g value)

instance Applicative Min where
  pure = Min
  Min f <*> Min value = Min (f value)

instance Monad Min where
  Min value >>= f = f value

instance Foldable Min where
  foldr f initial (Min value) = f value initial
  foldl f initial (Min value) = f initial value
  toList (Min value) = [value]
  null _ = False

instance Traversable Min where
  traverse f (Min value) = fmap Min (f value)

instance Applicative Max where
  pure = Max
  Max f <*> Max value = Max (f value)

instance Monad Max where
  Max value >>= f = f value

instance Foldable Max where
  foldr f initial (Max value) = f value initial
  foldl f initial (Max value) = f initial value
  toList (Max value) = [value]
  null _ = False

instance Traversable Max where
  traverse f (Max value) = fmap Max (f value)

instance Applicative First where
  pure = First
  First f <*> First value = First (f value)

instance Monad First where
  First value >>= f = f value

instance Foldable First where
  foldr f initial (First value) = f value initial
  foldl f initial (First value) = f initial value
  toList (First value) = [value]
  null _ = False

instance Traversable First where
  traverse f (First value) = fmap First (f value)

instance Applicative Last where
  pure = Last
  Last f <*> Last value = Last (f value)

instance Monad Last where
  Last value >>= f = f value

instance Foldable Last where
  foldr f initial (Last value) = f value initial
  foldl f initial (Last value) = f initial value
  toList (Last value) = [value]
  null _ = False

instance Traversable Last where
  traverse f (Last value) = fmap Last (f value)

instance Foldable (Arg a) where
  foldr f initial (Arg _ value) = f value initial
  foldl f initial (Arg _ value) = f initial value
  toList (Arg _ value) = [value]
  null _ = False

instance Traversable (Arg a) where
  traverse f (Arg key value) = fmap (Arg key) (f value)
