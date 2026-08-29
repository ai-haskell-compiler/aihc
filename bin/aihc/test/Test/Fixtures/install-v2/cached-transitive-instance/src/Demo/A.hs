module Demo.A
  ( Identity (..),
    Token (..),
  )
where

class Identity a where
  identity :: a -> a

data Token = Token

instance Identity Token where
  identity value = value
