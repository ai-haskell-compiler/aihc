module GHC.Show (Show (..)) where

import Prelude (String)

class Show a where
  show :: a -> String
