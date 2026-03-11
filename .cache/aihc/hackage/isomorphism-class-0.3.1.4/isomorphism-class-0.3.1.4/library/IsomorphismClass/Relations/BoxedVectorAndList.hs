{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndList where

import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo (Vector a) [a] where
  to = Data.Vector.fromList

instance IsomorphicTo [a] (Vector a) where
  to = Data.Vector.toList
