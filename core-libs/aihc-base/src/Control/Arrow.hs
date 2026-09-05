{-# LANGUAGE KindSignatures #-}

module Control.Arrow
  ( Arrow (..),
    (<<<),
    (>>>),
    returnA,
  )
where

import Control.Category (Category (..), (<<<), (>>>))
import Data.Kind (Type)

class (Category a) => Arrow (a :: Type -> Type -> Type) where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  (&&&) :: a b c -> a b c' -> a b (c, c')

infixr 3 ***

infixr 3 &&&

instance Arrow (->) where
  arr f = f
  first f (b, d) = (f b, d)
  second f (d, b) = (d, f b)
  (f *** g) (b, b') = (f b, g b')
  (f &&& g) b = (f b, g b)

returnA :: (Arrow a) => a b b
returnA = id
