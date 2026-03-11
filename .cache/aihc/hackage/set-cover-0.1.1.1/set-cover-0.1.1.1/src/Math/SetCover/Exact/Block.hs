module Math.SetCover.Exact.Block (blocksFromSets) where

import Math.SetCover.EnumMap (constMap)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Monoid.HT as Mn
import Data.Set (Set)
import Data.Tuple.HT (swap)
import Data.Bits (Bits, bitSize, setBit, shiftL, complement)


blocksFromSets ::
   (Ord a, Num block, Bits block) => [Set a] -> ([[block]], [block])
blocksFromSets sets =
   let dummyBlock = 0
       blockSize = bitSize dummyBlock
       complete = Set.unions sets
       mapToInt = Map.fromList $ zip (Set.toList complete) [0..]
       blocks =
         blocksFromInts blockSize .
         Map.elems . Map.intersection mapToInt . constMap ()
   in  (map blocks sets,
        case divMod (Set.size complete) blockSize of
            (numBlocks,remd) ->
               replicate numBlocks (complement 0 `asTypeOf` dummyBlock) ++
               Mn.when (remd>0) [shiftL 1 remd - 1])

blocksFromInts :: (Num block, Bits block) => Int -> [Int] -> [block]
blocksFromInts blockSize =
   zipWith blockFromBits (iterate (blockSize+) 0) . snd .
   flip (List.mapAccumL (\elems pivot -> swap $ span (<pivot) elems))
            (iterate (blockSize+) blockSize)

blockFromBits :: (Num block, Bits block) => Int -> [Int] -> block
blockFromBits offset = List.foldl' setBit 0 . map (subtract offset)
