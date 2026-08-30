module Prelude ((.)) where

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = compose
  where
    compose value = f (g value)

infixr 9 .
