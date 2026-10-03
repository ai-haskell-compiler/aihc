module Definitions (Describe (..), Token (..)) where

class Describe a where
  describe :: a -> a

data Token = Token
