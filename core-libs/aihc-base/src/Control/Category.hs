{-# LANGUAGE KindSignatures #-}

module Control.Category
  ( Category (..),
    (<<<),
    (>>>),
  )
where

import Data.Kind (Type)

class Category (cat :: Type -> Type -> Type) where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

infixr 9 .

instance Category (->) where
  id x = x
  (f . g) x = f (g x)

(<<<) :: (Category cat) => cat b c -> cat a b -> cat a c
(<<<) = (.)

infixr 1 <<<

(>>>) :: (Category cat) => cat a b -> cat b c -> cat a c
(>>>) inner outer = outer . inner

infixr 1 >>>
