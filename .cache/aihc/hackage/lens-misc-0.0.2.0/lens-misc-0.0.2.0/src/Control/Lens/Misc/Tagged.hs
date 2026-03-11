{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Lens.Misc.Tagged where

import Control.Lens
import Data.Tagged

_Tagged' :: Iso' (Tagged t a) a
_Tagged' = iso unTagged Tagged

_Tagged :: Iso (Tagged t a) (Tagged t b) a b
_Tagged = iso unTagged Tagged
