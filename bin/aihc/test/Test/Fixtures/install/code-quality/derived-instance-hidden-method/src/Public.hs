module Public (Wrap (..)) where

import Reexport

-- The derived instances define methods that no import brings into scope.
newtype Wrap a = Wrap {unWrap :: a}
  deriving (Eq, Ord, Show)
