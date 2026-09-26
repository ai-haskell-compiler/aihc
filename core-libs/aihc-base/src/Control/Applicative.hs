{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

module Control.Applicative
  ( Applicative (..),
    Alternative (..),
    Const (..),
    WrappedMonad (WrapMonad, unwrapMonad),
    WrappedArrow (WrapArrow, unwrapArrow),
    ZipList (..),
    liftA,
    liftA2,
    liftA3,
    optional,
    (<$>),
    (<**>),
  )
where

import Control.Arrow (Arrow (..), (>>>))
import Data.Semigroup.Internal (Monoid (..))
import Foreign.Storable (Storable (..))
import GHC.List (drop)
import GHC.Ptr (castPtr)
import Prelude (Applicative (..), Eq (..), Foldable (..), Functor (..), Maybe (..), Monad (..), Ord (..), Traversable (..), const, (++), (<$>))

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA = fmap

liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\value function -> function value)

infixl 4 <**>

-- | One occurrence of the action, or none.
optional :: (Alternative f) => f a -> f (Maybe a)
optional action = fmap Just action <|> pure Nothing

newtype Const a b = Const {getConst :: a}

-- | The phantom second parameter plays no part: a 'Const' compares as the
-- value it wraps.
instance (Eq a) => Eq (Const a b) where
  Const left == Const right = left == right
  Const left /= Const right = left /= right

instance (Ord a) => Ord (Const a b) where
  compare (Const left) (Const right) = compare left right
  Const left < Const right = left < right
  Const left <= Const right = left <= right
  Const left > Const right = left > right
  Const left >= Const right = left >= right
  max (Const left) (Const right) = Const (max left right)
  min (Const left) (Const right) = Const (min left right)

instance Functor (Const a) where
  fmap _ (Const value) = Const value

instance Foldable (Const a) where
  foldr _ initial _ = initial

instance Traversable (Const a) where
  traverse _ (Const value) = pure (Const value)

instance (Monoid a) => Applicative (Const a) where
  pure _ = Const mempty
  Const left <*> Const right = Const (left `mappend` right)

-- | Any 'Monad' can be made an 'Applicative' by sequencing its actions.
newtype WrappedMonad m a = WrapMonad {unwrapMonad :: m a}

instance (Monad m) => Functor (WrappedMonad m) where
  fmap f (WrapMonad action) = WrapMonad (action >>= \value -> return (f value))

instance (Monad m) => Applicative (WrappedMonad m) where
  pure value = WrapMonad (return value)
  WrapMonad functions <*> WrapMonad values =
    WrapMonad (functions >>= \function -> values >>= \value -> return (function value))

instance (Monad m) => Monad (WrappedMonad m) where
  WrapMonad action >>= f = WrapMonad (action >>= \value -> unwrapMonad (f value))

-- | Any 'Arrow' gives rise to an 'Applicative' in its output.
newtype WrappedArrow a b c = WrapArrow {unwrapArrow :: a b c}

instance (Arrow a) => Functor (WrappedArrow a b) where
  fmap f (WrapArrow arrow) = WrapArrow (arrow >>> arr f)

instance (Arrow a) => Applicative (WrappedArrow a b) where
  pure value = WrapArrow (arr (const value))
  WrapArrow functions <*> WrapArrow values =
    WrapArrow ((functions &&& values) >>> arr (\(function, value) -> function value))

newtype ZipList a = ZipList {getZipList :: [a]}
  deriving newtype (Eq, Ord, Functor)

instance Foldable ZipList where
  foldr step initial (ZipList values) = foldr step initial values
  length (ZipList values) = length values
  null (ZipList values) = null values

instance Traversable ZipList where
  traverse function (ZipList values) = ZipList <$> traverse function values

instance Applicative ZipList where
  pure value = ZipList (repeatZipList value)
  ZipList functions <*> ZipList values = ZipList (applyZipList functions values)

-- | The empty list is the identity. The alternative appends the part of
-- the second list that goes past the first one, so the length of the
-- result is the longer length.
instance Alternative ZipList where
  empty = ZipList []
  ZipList left <|> ZipList right = ZipList (left ++ drop (length left) right)

class (Applicative f) => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

  some value = fmap prepend value <*> many value
  many value = some value <|> pure []

infixl 3 <|>

prepend :: a -> [a] -> [a]
prepend value values = value : values

repeatZipList :: a -> [a]
repeatZipList value = value : repeatZipList value

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList [] _ = []
applyZipList _ [] = []
applyZipList (f : functions) (value : values) = f value : applyZipList functions values

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> value = value
  value <|> _ = value

-- | A 'Const' is stored as the value it wraps.
instance (Storable a) => Storable (Const a b) where
  sizeOf value = sizeOf (getConst value)
  alignment value = alignment (getConst value)
  peek address = fmap Const (peek (castPtr address))
  poke address (Const value) = poke (castPtr address) value
