{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Semigroup
  ( Semigroup (..),
    stimesMonoid,
    stimesIdempotent,
    stimesIdempotentMonoid,
    mtimesDefault,
    cycle1,
    diff,
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

import Control.Monad.Fix (MonadFix (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Function (fix)
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
import Prelude (Applicative (..), Bool (..), Bounded (..), Enum (..), Eq (..), Foldable (..), Functor (..), Integral, Monad (..), Monoid (..), Num, Ord (..), Ordering (..), Read, Show, Traversable (..), fmap, otherwise, (.))

-- | Repeat a value @n@ times. A count of zero gives 'mempty'.
mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault n x
  | n == 0 = mempty
  | otherwise = stimes n x

-- | Repeat a value without end.
cycle1 :: (Semigroup m) => m -> m
cycle1 xs = xs'
  where
    xs' = xs <> xs'

-- | Change a value into the 'Endo' that adds the value on the left.
diff :: (Semigroup m) => m -> Endo m
diff = Endo . (<>)

newtype Min a = Min {getMin :: a}
  deriving newtype (Num)

newtype Max a = Max {getMax :: a}
  deriving newtype (Num)

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

instance (Monoid m) => Semigroup (WrappedMonoid m) where
  WrapMonoid left <> WrapMonoid right = WrapMonoid (mappend left right)

deriving stock instance (Bounded a) => Bounded (Min a)

deriving stock instance (Bounded a) => Bounded (Max a)

deriving stock instance (Bounded a) => Bounded (First a)

deriving stock instance (Bounded a) => Bounded (Last a)

deriving stock instance (Bounded m) => Bounded (WrappedMonoid m)

deriving stock instance (Show a) => Show (Min a)

deriving stock instance (Show a) => Show (Max a)

deriving stock instance (Show a) => Show (First a)

deriving stock instance (Show a) => Show (Last a)

deriving stock instance (Show m) => Show (WrappedMonoid m)

deriving stock instance (Show a, Show b) => Show (Arg a b)

deriving stock instance (Read a) => Read (Min a)

deriving stock instance (Read a) => Read (Max a)

deriving stock instance (Read a) => Read (First a)

deriving stock instance (Read a) => Read (Last a)

deriving stock instance (Read m) => Read (WrappedMonoid m)

deriving stock instance (Read a, Read b) => Read (Arg a b)

instance (Enum a) => Enum (Min a) where
  succ (Min a) = Min (succ a)
  pred (Min a) = Min (pred a)
  toEnum = Min . toEnum
  fromEnum = fromEnum . getMin
  enumFrom (Min a) = fmap Min (enumFrom a)
  enumFromThen (Min a) (Min b) = fmap Min (enumFromThen a b)
  enumFromTo (Min a) (Min b) = fmap Min (enumFromTo a b)
  enumFromThenTo (Min a) (Min b) (Min c) = fmap Min (enumFromThenTo a b c)

instance (Enum a) => Enum (Max a) where
  succ (Max a) = Max (succ a)
  pred (Max a) = Max (pred a)
  toEnum = Max . toEnum
  fromEnum = fromEnum . getMax
  enumFrom (Max a) = fmap Max (enumFrom a)
  enumFromThen (Max a) (Max b) = fmap Max (enumFromThen a b)
  enumFromTo (Max a) (Max b) = fmap Max (enumFromTo a b)
  enumFromThenTo (Max a) (Max b) (Max c) = fmap Max (enumFromThenTo a b c)

instance (Enum a) => Enum (First a) where
  succ (First a) = First (succ a)
  pred (First a) = First (pred a)
  toEnum = First . toEnum
  fromEnum = fromEnum . getFirst
  enumFrom (First a) = fmap First (enumFrom a)
  enumFromThen (First a) (First b) = fmap First (enumFromThen a b)
  enumFromTo (First a) (First b) = fmap First (enumFromTo a b)
  enumFromThenTo (First a) (First b) (First c) = fmap First (enumFromThenTo a b c)

instance (Enum a) => Enum (Last a) where
  succ (Last a) = Last (succ a)
  pred (Last a) = Last (pred a)
  toEnum = Last . toEnum
  fromEnum = fromEnum . getLast
  enumFrom (Last a) = fmap Last (enumFrom a)
  enumFromThen (Last a) (Last b) = fmap Last (enumFromThen a b)
  enumFromTo (Last a) (Last b) = fmap Last (enumFromTo a b)
  enumFromThenTo (Last a) (Last b) (Last c) = fmap Last (enumFromThenTo a b c)

instance (Enum a) => Enum (WrappedMonoid a) where
  succ (WrapMonoid a) = WrapMonoid (succ a)
  pred (WrapMonoid a) = WrapMonoid (pred a)
  toEnum = WrapMonoid . toEnum
  fromEnum = fromEnum . unwrapMonoid
  enumFrom (WrapMonoid a) = fmap WrapMonoid (enumFrom a)
  enumFromThen (WrapMonoid a) (WrapMonoid b) = fmap WrapMonoid (enumFromThen a b)
  enumFromTo (WrapMonoid a) (WrapMonoid b) = fmap WrapMonoid (enumFromTo a b)
  enumFromThenTo (WrapMonoid a) (WrapMonoid b) (WrapMonoid c) =
    fmap WrapMonoid (enumFromThenTo a b c)

instance MonadFix Min where
  mfix f = fix (f . getMin)

instance MonadFix Max where
  mfix f = fix (f . getMax)

instance MonadFix First where
  mfix f = fix (f . getFirst)

instance MonadFix Last where
  mfix f = fix (f . getLast)

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
