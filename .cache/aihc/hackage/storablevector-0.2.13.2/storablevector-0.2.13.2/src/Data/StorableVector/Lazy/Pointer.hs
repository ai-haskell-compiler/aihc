{- |
In principle you can traverse through a lazy storable vector
using repeated calls to @viewL@.
However this needs a bit of pointer arrangement and allocation.
This data structure makes the inner loop faster,
that consists of traversing through a chunk.
-}
module Data.StorableVector.Lazy.Pointer (
   Pointer, cons, viewL, switchL,
   ) where

import Data.StorableVector.Lazy.PointerPrivate (Pointer(..), viewL, switchL, )
import qualified Data.StorableVector.Lazy as VL

import Foreign.Storable (Storable)


{-# INLINE cons #-}
cons :: Storable a => VL.Vector a -> Pointer a
cons = VL.pointer
