-- | A module of the program that "Main" does not import. The splices of
-- "Main" name its constructors and its class through "Helper".
module Tone (Tone (..), Label (..)) where

data Tone = Warm | Cold

class Label a where
  label :: a -> String
