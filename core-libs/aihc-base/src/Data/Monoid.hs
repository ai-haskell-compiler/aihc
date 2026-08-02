module Data.Monoid
  ( Monoid (..),
  )
where

import Data.Semigroup (Semigroup (..))
import Prelude (Maybe (..))

class (Semigroup a) => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

  mappend = (<>)
  mconcat = foldMonoid

infixr 6 `mappend`

{- HLINT ignore foldMonoid "Use foldr" -}
foldMonoid :: (Monoid a) => [a] -> a
foldMonoid [] = mempty
foldMonoid (value : values) = value <> foldMonoid values

instance Monoid [a] where
  mempty = []

instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing

instance Monoid () where
  mempty = ()

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
