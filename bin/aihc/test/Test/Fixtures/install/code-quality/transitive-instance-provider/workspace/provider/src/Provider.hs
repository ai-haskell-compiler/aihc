module Provider (Describe (..), Token (..)) where

class Describe a where
  describe :: a -> a

data Token = Token

instance Describe Token where
  describe Token = Token
