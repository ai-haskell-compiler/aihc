{- |
Alternative to "Math.SetCover.Queue.Set"
that represents sets by bit masks and uses a bitset-based Int priority queue.
-}
module Math.SetCover.Queue.BitPriorityQueue (Methods, methods) where

import qualified Math.SetCover.Queue as Queue
import Math.SetCover.Queue (SetId)

import qualified Math.SetCover.BitPriorityQueue as BitPQ
import qualified Math.SetCover.BitPosition as BitPos
import qualified Math.SetCover.BitSet as BitSet

import Data.Tuple.HT (mapFst)


type Methods bits = Queue.Methods (BitPQ.Queue bits SetId) (BitSet.Set bits)

methods :: BitPos.C bits => Methods bits
methods =
   Queue.Methods {
      Queue.fromEnumMap = BitPQ.fromSets,
      Queue.partition = (mapFst BitPQ.elemUnions.) . BitPQ.partition,
      Queue.difference = \q -> BitPQ.difference q . BitPQ.fromSets,
      Queue.findMinValue = BitPQ.findMinValue,
      Queue.null = BitPQ.null
   }
