{-# LANGUAGE PolyKinds #-}

module Data.Functor.Product
  ( Product (..),
  )
where

import Control.Applicative (Alternative (..))
import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))
import Prelude

data Product (f :: k -> Type) (g :: k -> Type) (a :: k) = Pair (f a) (g a)

instance (Eq (f a), Eq (g a)) => Eq (Product f g a) where
  Pair leftFirst leftSecond == Pair rightFirst rightSecond =
    leftFirst == rightFirst && leftSecond == rightSecond
  left /= right = not (left == right)

instance (Ord (f a), Ord (g a)) => Ord (Product f g a) where
  compare = compareProduct
  left < right = productComparisonLess (compareProduct left right)
  left <= right = productComparisonLessOrEqual (compareProduct left right)
  left > right = productComparisonGreater (compareProduct left right)
  left >= right = productComparisonGreaterOrEqual (compareProduct left right)
  max left right = productComparisonMax (compareProduct left right) left right
  min left right = productComparisonMin (compareProduct left right) left right

instance (Read (f a), Read (g a)) => Read (Product f g a) where
  readsPrec precedence = readParen (precedence > 10) readProduct

instance (Show (f a), Show (g a)) => Show (Product f g a) where
  showsPrec precedence (Pair first second) =
    showParen
      (precedence > 10)
      (showString "Pair " . showsPrec 11 first . showChar ' ' . showsPrec 11 second)

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Pair first second) = Pair (fmap f first) (fmap f second)

instance (Foldable f, Foldable g) => Foldable (Product f g) where
  foldMap f (Pair first second) = foldMap f first `mappend` foldMap f second
  foldr f initial (Pair first second) = foldr f (foldr f initial second) first
  foldl f initial (Pair first second) = foldl f (foldl f initial first) second

instance (Traversable f, Traversable g) => Traversable (Product f g) where
  traverse f (Pair first second) = fmap Pair (traverse f first) <*> traverse f second

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure value = Pair (pure value) (pure value)
  Pair firstFunctions secondFunctions <*> Pair firstValues secondValues =
    Pair (firstFunctions <*> firstValues) (secondFunctions <*> secondValues)

instance (Alternative f, Alternative g) => Alternative (Product f g) where
  empty = Pair empty empty
  Pair leftFirst leftSecond <|> Pair rightFirst rightSecond =
    Pair (leftFirst <|> rightFirst) (leftSecond <|> rightSecond)

instance (Monad f, Monad g) => Monad (Product f g) where
  Pair first second >>= next = Pair (first >>= firstProduct . next) (second >>= secondProduct . next)
  Pair first second >> Pair nextFirst nextSecond = Pair (first >> nextFirst) (second >> nextSecond)
  return value = Pair (return value) (return value)

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Product f g a) where
  Pair leftFirst leftSecond <> Pair rightFirst rightSecond =
    Pair (leftFirst <> rightFirst) (leftSecond <> rightSecond)

instance (Monoid (f a), Monoid (g a)) => Monoid (Product f g a) where
  mempty = Pair mempty mempty

firstProduct :: Product f g a -> f a
firstProduct (Pair first _) = first

secondProduct :: Product f g a -> g a
secondProduct (Pair _ second) = second

compareProduct :: (Ord (f a), Ord (g a)) => Product f g a -> Product f g a -> Ordering
compareProduct (Pair leftFirst leftSecond) (Pair rightFirst rightSecond) =
  case compare leftFirst rightFirst of
    LT -> LT
    EQ -> compare leftSecond rightSecond
    GT -> GT

productComparisonLess :: Ordering -> Bool
productComparisonLess LT = True
productComparisonLess _ = False

productComparisonLessOrEqual :: Ordering -> Bool
productComparisonLessOrEqual GT = False
productComparisonLessOrEqual _ = True

productComparisonGreater :: Ordering -> Bool
productComparisonGreater GT = True
productComparisonGreater _ = False

productComparisonGreaterOrEqual :: Ordering -> Bool
productComparisonGreaterOrEqual LT = False
productComparisonGreaterOrEqual _ = True

productComparisonMax :: Ordering -> a -> a -> a
productComparisonMax GT left _ = left
productComparisonMax _ _ right = right

productComparisonMin :: Ordering -> a -> a -> a
productComparisonMin GT _ right = right
productComparisonMin _ left _ = left

readProduct :: (Read (f a), Read (g a)) => ReadS (Product f g a)
readProduct input =
  case lex input of
    (constructor, afterConstructor) : _ ->
      case constructor == "Pair" of
        True -> readProductFirst (readsPrec 11 afterConstructor)
        False -> []
    [] -> []

readProductFirst :: (Read (g a)) => [(f a, String)] -> [(Product f g a, String)]
readProductFirst [] = []
readProductFirst ((first, afterFirst) : firstResults) =
  readProductSecond first (readsPrec 11 afterFirst) ++ readProductFirst firstResults

readProductSecond :: f a -> [(g a, String)] -> [(Product f g a, String)]
readProductSecond _ [] = []
readProductSecond first ((second, rest) : secondResults) =
  (Pair first second, rest) : readProductSecond first secondResults
