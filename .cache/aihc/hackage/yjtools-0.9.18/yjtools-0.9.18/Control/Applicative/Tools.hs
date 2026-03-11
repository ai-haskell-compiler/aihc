module Control.Applicative.Tools (
  (<.>)
) where

infixr 9 <.>
(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2
