{- |
Alternative to "Math.SetCover.Exact" that uses a priority queue
and avoids full scans through available sets.
-}
module Math.SetCover.Queue.Set (Methods, methods) where

import qualified Math.SetCover.Queue as Queue
import Math.SetCover.Queue (SetId)

import qualified Math.SetCover.EnumMap as EnumMapX
import qualified Data.OrdPSQ as PSQ
import qualified Data.EnumSet as EnumSet; import Data.EnumSet (EnumSet)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.EnumMap (EnumMap)
import Data.Tuple.HT (swap, mapFst)


type Methods a = Queue.Methods (PSQ.OrdPSQ a Int (EnumSet SetId)) (Set.Set a)

methods :: Ord a => Methods a
methods =
   Queue.Methods {
      Queue.fromEnumMap =
         PSQ.fromList . map (\(elm, ns) -> (elm, EnumSet.size ns, ns)) .
         Map.toList . EnumMapX.transposeSet,
      Queue.partition =
         \q -> mapFst EnumSet.unions . partitionPSQ q . Set.toList,
      Queue.difference = differencePSQ,
      Queue.findMinValue =
         fmap (\(elm, _, ns) -> (Set.singleton elm, ns)) . PSQ.findMin,
      Queue.null = PSQ.null
   }

{- |
The list of keys must be a subset of the queue keys.
-}
partitionPSQ ::
   (Ord k, Ord p) => PSQ.OrdPSQ k p v -> [k] -> ([v], PSQ.OrdPSQ k p v)
partitionPSQ =
   (swap .) .
   List.mapAccumL
      (\q0 k ->
         maybe
            (error "partitionPSQ: key not contained in queue's key set")
            (\(_p,v,q1) -> (q1, v)) $
         PSQ.deleteView k q0)

differencePSQ, _differencePSQ ::
   (Ord k, Enum e) =>
   PSQ.OrdPSQ k Int (EnumSet e) ->
   EnumMap e (Set.Set k) -> PSQ.OrdPSQ k Int (EnumSet e)
differencePSQ q =
   foldl (flip deleteSetFromPSQ) q . Map.toList . EnumMapX.transposeSet

_differencePSQ q =
   Map.foldlWithKey (curry . flip deleteSetFromPSQ) q . EnumMapX.transposeSet

deleteSetFromPSQ ::
   (Ord k) =>
   (k, EnumSet e) -> PSQ.OrdPSQ k Int (EnumSet e) ->
   PSQ.OrdPSQ k Int (EnumSet e)
deleteSetFromPSQ (elm, ns) =
   updatePSQ (flip differenceSizedSet ns) elm

differenceSizedSet :: (Int, EnumSet e) -> EnumSet e -> (Int, EnumSet e)
differenceSizedSet (size, a) b =
   let section = EnumSet.intersection a b
   in  (size - EnumSet.size section, EnumSet.difference a section)

updatePSQ ::
   (Ord p, Ord k) =>
   ((p, v) -> (p, v)) -> k -> PSQ.OrdPSQ k p v -> PSQ.OrdPSQ k p v
updatePSQ f k = snd . PSQ.alter ((,) () . fmap f) k
