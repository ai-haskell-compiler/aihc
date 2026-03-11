{- |
This implementation uses priority queues
and avoids full scans through available sets.
It can be faster than "Math.SetCover.Exact"
if there is a huge number of sets.
-}
module Math.SetCover.Exact.Priority (
   Assign, ESC.label, ESC.labeledSet, ESC.assign,
   partitions, search, step,
   State(..), initState, updateState,
   Tree(..), decisionTree, completeTree,
   SetId, queueMap, queueSet, queueBit, queueBitPQ, queueIntSet,
   ) where

import qualified Math.SetCover.Queue.Map as QueueMap
import qualified Math.SetCover.Queue.Set as QueueSet
import qualified Math.SetCover.Queue.Bit as QueueBit
import qualified Math.SetCover.Queue.BitPriorityQueue as QueueBitPQ

import qualified Math.SetCover.BitPosition as BitPos

import qualified Math.SetCover.Queue as Queue
import qualified Math.SetCover.Exact as ESC
import Math.SetCover.Queue (Methods, SetId(SetId))
import Math.SetCover.Exact (Assign(Assign), labeledSet, Tree(Branch,Leaf))

import qualified Math.SetCover.EnumMap as EnumMapX
import qualified Data.EnumMap as EnumMap; import Data.EnumMap (EnumMap)
import qualified Data.Foldable as Fold
import Data.EnumSet (EnumSet)
import Data.Tuple.HT (mapSnd)


data State queue label set =
   State {
      availableSubsets :: EnumMap SetId (Assign label set),
      queue :: queue,
      usedSubsets :: [label]
   }

initState ::
   Methods queue set -> [Assign label set] -> State queue label set
initState dict subsets =
   let numberedSets = EnumMap.fromList $ zip [SetId 0 ..] subsets
   in  State {
         availableSubsets = numberedSets,
         queue = Queue.fromEnumMap dict $ fmap labeledSet numberedSets,
         usedSubsets = []
       }

{-# INLINE updateState #-}
updateState ::
   Methods queue set ->
   Assign label set -> State queue label set -> State queue label set
updateState dict (Assign attemptLabel attemptedSet) s =
   let (attemptElems, remainingQueue) =
         Queue.partition dict (queue s) attemptedSet
       (removed, remaining) =
         EnumMapX.partition (availableSubsets s) attemptElems
   in  State {
         availableSubsets = remaining,
         queue = Queue.difference dict remainingQueue $ fmap labeledSet removed,
         usedSubsets = attemptLabel : usedSubsets s
       }

{-# INLINE nextStates #-}
nextStates ::
   Methods queue set ->
   State queue label set ->
   EnumSet SetId -> [State queue label set]
nextStates dict s =
   map (flip (updateState dict) s) . EnumMap.elems .
   EnumMapX.intersection (availableSubsets s)

{-# INLINE step #-}
step :: Methods queue set -> State queue label set -> [State queue label set]
step dict s =
   flip Fold.foldMap (Queue.findMin dict (queue s)) $ nextStates dict s

{-# INLINE search #-}
search :: Methods queue set -> State queue label set -> [[label]]
search dict =
   let go s =
         case Queue.findMin dict (queue s) of
            Nothing -> [usedSubsets s]
            Just setIds -> nextStates dict s setIds >>= go
   in  go

{-# INLINE partitions #-}
partitions :: Methods queue set -> [Assign label set] -> [[label]]
partitions dict = search dict . initState dict



completeTree :: Methods queue set -> State queue label set -> Tree label set
completeTree dict =
   let go s =
         case Queue.findMinValue dict (queue s) of
            Nothing -> Leaf
            Just mins ->
               uncurry Branch $ flip mapSnd mins $
                  map (\asn -> (ESC.label asn, go $ updateState dict asn s)) .
                  EnumMap.elems . EnumMapX.intersection (availableSubsets s)
   in  go

decisionTree :: Methods queue set -> [Assign label set] -> Tree label set
decisionTree dict = completeTree dict . initState dict


-- * different priority queue implementations

queueMap :: Ord a => Queue.Methods queue set -> QueueMap.Methods a queue set
queueMap = QueueMap.methods

queueSet :: Ord a => QueueSet.Methods a
queueSet = QueueSet.methods

queueBit :: BitPos.C bits => QueueBit.Methods bits
queueBit = QueueBit.methods

queueIntSet :: QueueBit.MethodsIntSet
queueIntSet = QueueBit.methodsIntSet

queueBitPQ :: BitPos.C bits => QueueBitPQ.Methods bits
queueBitPQ = QueueBitPQ.methods
