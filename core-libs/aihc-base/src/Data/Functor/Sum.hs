{-# LANGUAGE PolyKinds #-}

module Data.Functor.Sum
  ( Sum (..),
  )
where

import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.Traversable (Traversable (..))
import Prelude

data Sum (f :: k -> Type) (g :: k -> Type) (a :: k) = InL (f a) | InR (g a)

instance (Eq (f a), Eq (g a)) => Eq (Sum f g a) where
  InL left == InL right = left == right
  InL _ == InR _ = False
  InR _ == InL _ = False
  InR left == InR right = left == right
  left /= right = not (left == right)

instance (Ord (f a), Ord (g a)) => Ord (Sum f g a) where
  compare = compareSum
  left < right = sumComparisonLess (compareSum left right)
  left <= right = sumComparisonLessOrEqual (compareSum left right)
  left > right = sumComparisonGreater (compareSum left right)
  left >= right = sumComparisonGreaterOrEqual (compareSum left right)
  max left right = sumComparisonMax (compareSum left right) left right
  min left right = sumComparisonMin (compareSum left right) left right

instance (Read (f a), Read (g a)) => Read (Sum f g a) where
  readsPrec precedence input =
    readParen (precedence > 10) readLeft input ++ readParen (precedence > 10) readRight input
    where
      readLeft source =
        case lex source of
          (constructor, afterConstructor) : _ ->
            case constructor == "InL" of
              True -> wrapLeftReads (readsPrec 11 afterConstructor)
              False -> []
          [] -> []
      readRight source =
        case lex source of
          (constructor, afterConstructor) : _ ->
            case constructor == "InR" of
              True -> wrapRightReads (readsPrec 11 afterConstructor)
              False -> []
          [] -> []

instance (Show (f a), Show (g a)) => Show (Sum f g a) where
  showsPrec precedence (InL value) =
    showParen (precedence > 10) (showString "InL " . showsPrec 11 value)
  showsPrec precedence (InR value) =
    showParen (precedence > 10) (showString "InR " . showsPrec 11 value)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (InL value) = InL (fmap f value)
  fmap f (InR value) = InR (fmap f value)

instance (Foldable f, Foldable g) => Foldable (Sum f g) where
  foldMap f (InL value) = foldMap f value
  foldMap f (InR value) = foldMap f value
  foldr f initial (InL value) = foldr f initial value
  foldr f initial (InR value) = foldr f initial value
  foldl f initial (InL value) = foldl f initial value
  foldl f initial (InR value) = foldl f initial value

instance (Traversable f, Traversable g) => Traversable (Sum f g) where
  traverse f (InL value) = fmap InL (traverse f value)
  traverse f (InR value) = fmap InR (traverse f value)

compareSum :: (Ord (f a), Ord (g a)) => Sum f g a -> Sum f g a -> Ordering
compareSum (InL left) (InL right) = compare left right
compareSum (InL _) (InR _) = LT
compareSum (InR _) (InL _) = GT
compareSum (InR left) (InR right) = compare left right

sumComparisonLess :: Ordering -> Bool
sumComparisonLess LT = True
sumComparisonLess _ = False

sumComparisonLessOrEqual :: Ordering -> Bool
sumComparisonLessOrEqual GT = False
sumComparisonLessOrEqual _ = True

sumComparisonGreater :: Ordering -> Bool
sumComparisonGreater GT = True
sumComparisonGreater _ = False

sumComparisonGreaterOrEqual :: Ordering -> Bool
sumComparisonGreaterOrEqual LT = False
sumComparisonGreaterOrEqual _ = True

sumComparisonMax :: Ordering -> a -> a -> a
sumComparisonMax GT left _ = left
sumComparisonMax _ _ right = right

sumComparisonMin :: Ordering -> a -> a -> a
sumComparisonMin GT _ right = right
sumComparisonMin _ left _ = left

wrapLeftReads :: [(f a, String)] -> [(Sum f g a, String)]
wrapLeftReads [] = []
wrapLeftReads ((value, rest) : results) = (InL value, rest) : wrapLeftReads results

wrapRightReads :: [(g a, String)] -> [(Sum f g a, String)]
wrapRightReads [] = []
wrapRightReads ((value, rest) : results) = (InR value, rest) : wrapRightReads results
