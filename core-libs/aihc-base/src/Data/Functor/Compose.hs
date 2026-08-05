{-# LANGUAGE PolyKinds #-}

module Data.Functor.Compose
  ( Compose (..),
  )
where

import Control.Applicative (Alternative (..))
import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))
import Prelude

newtype Compose (f :: k -> Type) (g :: l -> k) (a :: l) = Compose {getCompose :: f (g a)}

infixr 9 `Compose`

instance (Eq (f (g a))) => Eq (Compose f g a) where
  Compose left == Compose right = left == right
  Compose left /= Compose right = left /= right

instance (Ord (f (g a))) => Ord (Compose f g a) where
  compare (Compose left) (Compose right) = compare left right
  Compose left < Compose right = left < right
  Compose left <= Compose right = left <= right
  Compose left > Compose right = left > right
  Compose left >= Compose right = left >= right
  max (Compose left) (Compose right) = Compose (max left right)
  min (Compose left) (Compose right) = Compose (min left right)

instance (Bounded (f (g a))) => Bounded (Compose f g a) where
  minBound = Compose minBound
  maxBound = Compose maxBound

instance (Enum (f (g a))) => Enum (Compose f g a) where
  succ (Compose value) = Compose (succ value)
  pred (Compose value) = Compose (pred value)
  toEnum value = Compose (toEnum value)
  fromEnum (Compose value) = fromEnum value
  enumFrom (Compose value) = mapCompose (enumFrom value)
  enumFromThen (Compose first) (Compose second) = mapCompose (enumFromThen first second)
  enumFromTo (Compose first) (Compose last) = mapCompose (enumFromTo first last)
  enumFromThenTo (Compose first) (Compose second) (Compose last) =
    mapCompose (enumFromThenTo first second last)

instance (Read (f (g a))) => Read (Compose f g a) where
  readsPrec precedence = readParen (precedence > 10) readCompose

instance (Show (f (g a))) => Show (Compose f g a) where
  showsPrec precedence (Compose value) =
    showParen (precedence > 10) (showString "Compose " . showsPrec 11 value)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose value) = Compose (fmap (fmap f) value)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose value) = foldMap (foldMap f) value
  foldr f initial (Compose value) = foldr (foldComposeRight f) initial value
  foldl f initial (Compose value) = foldl (foldl f) initial value

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose value) = fmap Compose (traverse (traverse f) value)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure value = Compose (pure (pure value))
  Compose functions <*> Compose values = Compose (fmap (<*>) functions <*> values)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
  empty = Compose empty
  Compose left <|> Compose right = Compose (left <|> right)

instance (Semigroup (f (g a))) => Semigroup (Compose f g a) where
  Compose left <> Compose right = Compose (left <> right)

instance (Monoid (f (g a))) => Monoid (Compose f g a) where
  mempty = Compose mempty

readCompose :: (Read (f (g a))) => ReadS (Compose f g a)
readCompose input =
  case lex input of
    (constructor, afterConstructor) : _ ->
      case constructor == "Compose" of
        True -> wrapComposeReads (readsPrec 11 afterConstructor)
        False -> []
    [] -> []

wrapComposeReads :: [(f (g a), String)] -> [(Compose f g a, String)]
wrapComposeReads [] = []
wrapComposeReads ((value, rest) : results) = (Compose value, rest) : wrapComposeReads results

mapCompose :: [f (g a)] -> [Compose f g a]
mapCompose = fmap Compose

foldComposeRight :: (Foldable g) => (a -> b -> b) -> g a -> b -> b
foldComposeRight f inner rest = foldr f rest inner
