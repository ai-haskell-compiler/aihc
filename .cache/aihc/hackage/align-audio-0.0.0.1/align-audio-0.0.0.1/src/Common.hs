module Common where

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Data.Array.Comfort.Storable (Array)

import Foreign.Storable (Storable)


cyclicFromVector :: (Storable a) => SVL.Vector a -> Array (Shape.Cyclic Int) a
cyclicFromVector =
   Array.mapShape (\(Shape.ZeroBased n) -> Shape.Cyclic n) .
   Array.fromStorableVector . SV.concat . SVL.chunks

pad :: (Storable a, Num a) => Int -> SVL.Vector a -> SVL.Vector a
pad n xs =
   SVL.append xs $ SVL.replicate SVL.defaultChunkSize (n - SVL.length xs) 0
