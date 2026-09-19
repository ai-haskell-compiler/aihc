module Data.Semigroup.Internal
  ( Semigroup (..),
    Monoid (..),
    stimesDefault,
    stimesMonoid,
    stimesIdempotent,
    stimesIdempotentMonoid,
    Dual (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
  )
where

import Data.Bool (Bool (..), (&&), (||))
import GHC.Base (Functor (..), List (..), Maybe (..))
import GHC.Err (errorWithoutStackTrace)
import GHC.Internal.Classes (Eq (..), Ord (..), Ordering (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import GHC.Num (Num (..))
import GHC.Real (Integral)

class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: (Integral b) => b -> a -> a

  sconcat (value :| values) = sconcatList value values
  stimes = stimesDefault

infixr 6 <>

class (Semigroup a) => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

  mappend = (<>)
  mconcat = foldMonoid

infixr 6 `mappend`

sconcatList :: (Semigroup a) => a -> [a] -> a
sconcatList value [] = value
sconcatList value (next : rest) = value <> sconcatList next rest

{- HLINT ignore foldMonoid "Use foldr" -}
foldMonoid :: (Monoid a) => [a] -> a
foldMonoid [] = mempty
foldMonoid (value : values) = value <> foldMonoid values

-- | Repeat a semigroup value a positive number of times.
stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
stimesDefault count value =
  if count <= 0
    then errorWithoutStackTrace "stimes: positive multiplier expected"
    else stimesPositive count value

stimesPositive :: (Integral b, Semigroup a) => b -> a -> a
stimesPositive count value =
  if count == 1
    then value
    else value <> stimesPositive (count - 1) value

-- | Repeat a monoid value a non-negative number of times.
stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid count value =
  case compare count 0 of
    LT -> errorWithoutStackTrace "stimesMonoid: negative multiplier"
    EQ -> mempty
    GT -> stimesPositive count value

stimesIdempotent :: (Integral b) => b -> a -> a
stimesIdempotent count value =
  if count <= 0
    then errorWithoutStackTrace "stimesIdempotent: positive multiplier expected"
    else value

stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid count value =
  case compare count 0 of
    LT -> errorWithoutStackTrace "stimesIdempotentMonoid: negative multiplier"
    EQ -> mempty
    GT -> value

instance Semigroup [a] where
  (<>) = appendList

{- HLINT ignore appendList "Use foldr" -}
appendList :: [a] -> [a] -> [a]
appendList [] right = right
appendList (value : values) right = value : appendList values right

instance Monoid [a] where
  mempty = []

instance (Semigroup a) => Semigroup (Maybe a) where
  Nothing <> value = value
  value <> Nothing = value
  Just left <> Just right = Just (left <> right)

instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing

instance Semigroup () where
  _ <> _ = ()

instance Monoid () where
  mempty = ()

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (leftA, leftB) <> (rightA, rightB) = (leftA <> rightA, leftB <> rightB)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> right = right
  GT <> _ = GT

instance Monoid Ordering where
  mempty = EQ

instance (Semigroup b) => Semigroup (a -> b) where
  (left <> right) value = left value <> right value

instance (Monoid b) => Monoid (a -> b) where
  mempty _ = mempty

-- | The wrappers that only change how values combine live here so that both
-- "Data.Monoid" and "Data.Semigroup" can re-export them.
newtype Dual a = Dual {getDual :: a}

newtype All = All {getAll :: Bool}

newtype Any = Any {getAny :: Bool}

newtype Sum a = Sum {getSum :: a}

newtype Product a = Product {getProduct :: a}

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

instance Functor Dual where
  fmap f (Dual value) = Dual (f value)

instance Functor Sum where
  fmap f (Sum value) = Sum (f value)

instance Functor Product where
  fmap f (Product value) = Product (f value)
