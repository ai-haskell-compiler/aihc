module Control.Applicative
  ( Applicative (..),
    Alternative (..),
  )
where

import Prelude (Applicative (..), Functor (..), Maybe (..), (++))

class (Applicative f) => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

  some value = fmap prepend value <*> many value
  many value = some value <|> pure []

infixl 3 <|>

prepend :: a -> [a] -> [a]
prepend value values = value : values

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> value = value
  value <|> _ = value
