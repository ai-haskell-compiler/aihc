module VectorExtras.Generic
  ( fromFoldable,
    fromFoldableNarrowing,
    fromReverseListN,
    fromAssocListWithGen,
    fromAssocListWithDef,
    initialized,
    chunk,
  )
where

import Base.Generic
import Base.Prelude hiding (length)
import Data.Vector.Generic hiding (foldl')
import qualified VectorExtras.Accumulator as VAcc

{-# INLINE fromFoldable #-}
fromFoldable :: (Foldable f, Vector v a) => f a -> v a
fromFoldable =
  VAcc.toVector . foldl' (flip VAcc.add) VAcc.init

{-# INLINE fromFoldableNarrowing #-}
fromFoldableNarrowing :: (Foldable f, Vector v b) => (a -> Maybe b) -> f a -> v b
fromFoldableNarrowing narrow =
  VAcc.toVector . foldl' step VAcc.init
  where
    step acc el = case narrow el of
      Nothing -> acc
      Just narrowed -> VAcc.add narrowed acc

-- |
-- Distribute the elements of a vector across the specified amount of chunks.
--
-- Depending on the size of the vector the first chunks may be larger in size then the others by one.
chunk :: (Vector v1 a, Vector v2 (v1 a)) => Int -> v1 a -> v2 (v1 a)
chunk chunksAmount vector =
  let vectorLength = length vector
      smallerChunkSize = div vectorLength chunksAmount
      largerChunkSize = succ smallerChunkSize
      largerChunksAmount = vectorLength - smallerChunkSize * chunksAmount
      largerChunksElementsAmount = largerChunksAmount * largerChunkSize
   in generate chunksAmount $ \chunkIndex ->
        let chunkOriginalIndex =
              if largerChunksAmount > chunkIndex
                then chunkIndex * largerChunkSize
                else largerChunksElementsAmount + (chunkIndex - largerChunksAmount) * smallerChunkSize
         in generate (if chunkIndex < largerChunksAmount then largerChunkSize else smallerChunkSize) $ \elemIndex ->
              unsafeIndex vector
                $ chunkOriginalIndex
                + elemIndex
