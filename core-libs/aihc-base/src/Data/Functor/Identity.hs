{-# LANGUAGE KindSignatures #-}

module Data.Functor.Identity
  ( Identity (..),
  )
where

import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))
import Prelude

newtype Identity (a :: Type) = Identity {runIdentity :: a}

instance (Eq a) => Eq (Identity a) where
  Identity left == Identity right = left == right
  Identity left /= Identity right = left /= right

instance (Ord a) => Ord (Identity a) where
  compare (Identity left) (Identity right) = compare left right
  Identity left < Identity right = left < right
  Identity left <= Identity right = left <= right
  Identity left > Identity right = left > right
  Identity left >= Identity right = left >= right
  max (Identity left) (Identity right) = Identity (max left right)
  min (Identity left) (Identity right) = Identity (min left right)

instance (Bounded a) => Bounded (Identity a) where
  minBound = Identity minBound
  maxBound = Identity maxBound

instance (Enum a) => Enum (Identity a) where
  succ (Identity value) = Identity (succ value)
  pred (Identity value) = Identity (pred value)
  toEnum value = Identity (toEnum value)
  fromEnum (Identity value) = fromEnum value
  enumFrom (Identity value) = mapIdentity (enumFrom value)
  enumFromThen (Identity first) (Identity second) = mapIdentity (enumFromThen first second)
  enumFromTo (Identity first) (Identity last) = mapIdentity (enumFromTo first last)
  enumFromThenTo (Identity first) (Identity second) (Identity last) =
    mapIdentity (enumFromThenTo first second last)

instance (Read a) => Read (Identity a) where
  readsPrec precedence = readParen (precedence > 10) readIdentity

instance (Show a) => Show (Identity a) where
  showsPrec precedence (Identity value) =
    showParen (precedence > 10) (showString "Identity " . showsPrec 11 value)

instance Functor Identity where
  fmap f (Identity value) = Identity (f value)

instance Foldable Identity where
  foldMap f (Identity value) = f value
  foldr f initial (Identity value) = f value initial
  foldl f initial (Identity value) = f initial value
  null _ = False
  length _ = 1

instance Traversable Identity where
  traverse f (Identity value) = fmap Identity (f value)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity value = Identity (f value)

instance Monad Identity where
  Identity value >>= next = next value
  Identity _ >> next = next
  return = Identity

instance (Semigroup a) => Semigroup (Identity a) where
  Identity left <> Identity right = Identity (left <> right)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

readIdentity :: (Read a) => ReadS (Identity a)
readIdentity input =
  case lex input of
    (constructor, afterConstructor) : _ ->
      case constructor == "Identity" of
        True -> wrapIdentityReads (readsPrec 11 afterConstructor)
        False -> []
    [] -> []

wrapIdentityReads :: [(a, String)] -> [(Identity a, String)]
wrapIdentityReads [] = []
wrapIdentityReads ((value, rest) : results) = (Identity value, rest) : wrapIdentityReads results

mapIdentity :: [a] -> [Identity a]
mapIdentity = fmap Identity
