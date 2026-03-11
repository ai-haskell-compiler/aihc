{- |
Alternative to "Math.SetCover.Queue.Set"
that represents sets by bit masks and uses the faster Int priority queue.
-}
module Math.SetCover.Queue.Bit (
   Methods, methods,
   MethodsIntSet, methodsIntSet,
   ) where

import qualified Math.SetCover.Queue as Queue
import Math.SetCover.Queue (SetId)

import qualified Math.SetCover.EnumMap as EnumMapX
import qualified Math.SetCover.BitPosition as BitPos
import qualified Math.SetCover.BitSet as BitSet

import qualified Data.IntPSQ as PSQ
import qualified Data.EnumSet as EnumSet; import Data.EnumSet (EnumSet)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.IntSet (IntSet)
import Data.Tuple.HT (swap, mapFst)


type
   Methods bits =
      Queue.Methods (PSQ.IntPSQ Int (EnumSet SetId)) (BitSet.Set bits)

methods :: BitPos.C bits => Methods bits
methods =
   Queue.Methods {
      Queue.fromEnumMap =
         PSQ.fromList . map (\(elm, ns) -> (elm, EnumSet.size ns, ns)) .
         IntMap.toList . EnumMapX.transposeBitSet,
      Queue.partition =
         \q -> mapFst EnumSet.unions . partitionPSQ q . BitPos.unpack,
      Queue.difference = \q ->
         foldl (flip deleteSetFromPSQ) q .
         IntMap.toList . EnumMapX.transposeBitSet,
      Queue.findMinValue =
         fmap (\(elm, _, ns) -> (BitPos.singleton elm, ns)) . PSQ.findMin,
      Queue.null = PSQ.null
   }


type MethodsIntSet = Queue.Methods (PSQ.IntPSQ Int (EnumSet SetId)) IntSet

methodsIntSet :: MethodsIntSet
methodsIntSet =
   Queue.Methods {
      Queue.fromEnumMap =
         PSQ.fromList . map (\(elm, ns) -> (elm, EnumSet.size ns, ns)) .
         IntMap.toList . EnumMapX.transposeIntSet,
      Queue.partition =
         \q -> mapFst EnumSet.unions . partitionPSQ q . IntSet.toList,
      Queue.difference = \q ->
         foldl (flip deleteSetFromPSQ) q .
         IntMap.toList . EnumMapX.transposeIntSet,
      Queue.findMinValue =
         fmap (\(elm, _, ns) -> (IntSet.singleton elm, ns)) . PSQ.findMin,
      Queue.null = PSQ.null
   }


{- |
The list of keys must be a subset of the queue keys.
-}
partitionPSQ :: (Ord p) => PSQ.IntPSQ p v -> [Int] -> ([v], PSQ.IntPSQ p v)
partitionPSQ =
   (swap .) .
   List.mapAccumL
      (\q0 k ->
         maybe
            (error "partitionPSQ: key not contained in queue's key set")
            (\(_p,v,q1) -> (q1, v)) $
         PSQ.deleteView k q0)

deleteSetFromPSQ ::
   (Int, EnumSet e) -> PSQ.IntPSQ Int (EnumSet e) ->
   PSQ.IntPSQ Int (EnumSet e)
deleteSetFromPSQ (elm, ns) =
   updatePSQ (flip differenceSizedSet ns) elm

differenceSizedSet :: (Int, EnumSet e) -> EnumSet e -> (Int, EnumSet e)
differenceSizedSet (size, a) b =
   let section = EnumSet.intersection a b
   in  (size - EnumSet.size section, EnumSet.difference a section)

updatePSQ ::
   (Ord p) => ((p, v) -> (p, v)) -> Int -> PSQ.IntPSQ p v -> PSQ.IntPSQ p v
updatePSQ f k = snd . PSQ.alter ((,) () . fmap f) k
