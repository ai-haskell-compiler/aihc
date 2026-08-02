module Data.Semigroup
  ( Semigroup (..),
  )
where

import Prelude (Maybe (..), (++))

class Semigroup a where
  (<>) :: a -> a -> a

infixr 6 <>

instance Semigroup [a] where
  (<>) = (++)

instance (Semigroup a) => Semigroup (Maybe a) where
  Nothing <> value = value
  value <> Nothing = value
  Just left <> Just right = Just (left <> right)

instance Semigroup () where
  _ <> _ = ()

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (leftA, leftB) <> (rightA, rightB) = (leftA <> rightA, leftB <> rightB)
