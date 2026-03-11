module Control.Isomorphism.Partial.Unsafe
  ( Iso (Iso)
  ) where

import Prelude ()
import Data.Maybe (Maybe ())

data Iso alpha beta 
  = Iso (alpha -> Maybe beta) (beta -> Maybe alpha)
