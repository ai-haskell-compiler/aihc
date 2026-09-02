{-# LANGUAGE KindSignatures #-}

module Control.Category
  ( Category (..),
    (<<<),
    (>>>),
  )
where

import Data.Kind (Type)

-- | The function arrow has no instance. The type checker does not unify
-- the arrow constructor with function types.
class Category (cat :: Type -> Type -> Type) where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

infixr 9 .

(<<<) :: (Category cat) => cat b c -> cat a b -> cat a c
(<<<) = (.)

infixr 1 <<<

(>>>) :: (Category cat) => cat a b -> cat b c -> cat a c
(>>>) inner outer = outer . inner

infixr 1 >>>
