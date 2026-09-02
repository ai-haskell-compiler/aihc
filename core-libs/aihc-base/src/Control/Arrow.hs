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

-- | The function arrow has no instance. The type checker does not unify
-- the arrow constructor with function types.
class (Category a) => Arrow (a :: Type -> Type -> Type) where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  (&&&) :: a b c -> a b c' -> a b (c, c')

infixr 3 ***

infixr 3 &&&

returnA :: (Arrow a) => a b b
returnA = id
