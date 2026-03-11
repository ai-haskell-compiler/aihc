module Math.SetCover.BitPriorityQueue (
   Queue,
   null,
   fromSets,
   elemUnions,
   partition,
   difference,
   findMin,
   findMinValue,
   ) where

import qualified Math.SetCover.EnumMap as EnumMapX
import qualified Math.SetCover.BitPosition as BitPos
import qualified Math.SetCover.BitMap as BitMap
import qualified Math.SetCover.BitSet as BitSet
import Math.SetCover.EnumMap (constIntMapFromBits)

import qualified Data.EnumSet as EnumSet; import Data.EnumSet (EnumSet)
import qualified Data.IntMap as IntMap; import Data.IntMap (IntMap)
import qualified Data.Foldable as Fold
import Data.EnumMap (EnumMap)
import Data.Monoid (mempty, mconcat)
import Data.Maybe.HT (toMaybe)

import Prelude hiding (null)


{-
We could generalize @EnumSet e@ to @a@
and pretend that the priorities are independent of the 'EnumSet' sizes.
However, 'difference' makes only sense if the priorities match the set sizes.
-}
data Queue bits e = Queue (BitMap.Map bits) (IntMap (EnumSet e))

null :: Queue bits e -> Bool
null (Queue _ns m) = IntMap.null m

fromSets ::
   (Enum e, BitPos.C bits) => EnumMap e (BitSet.Set bits) -> Queue bits e
fromSets xs =
   Queue
      (Fold.foldl' (flip BitMap.inc) mempty xs)
      (EnumMapX.transposeBitSet xs)

elemUnions :: (Enum e) => Queue t e -> EnumSet e
elemUnions (Queue _ns m) = Fold.fold m

keysBits :: (BitPos.C bits) => Queue bits e -> BitSet.Set bits
keysBits (Queue _ m) =
   mconcat $ map BitPos.singleton $ IntMap.keys m

findMin :: (BitPos.C bits) => Queue bits e -> Maybe (EnumSet e)
findMin = fmap snd . findMinValue

findMinValue ::
   (BitPos.C bits) => Queue bits e -> Maybe (BitSet.Set bits, EnumSet e)
findMinValue q@(Queue ns m) =
   let used = keysBits q
       minSet = BitSet.keepMinimum $ BitMap.minimumSet used ns
   in  toMaybe (not $ BitSet.null used) $ (,) minSet $
          IntMap.findWithDefault
             (error "findMin: key with minimal priority must be in IntMap")
             (BitPos.bitPosition minSet)
             m

difference ::
   (BitPos.C bits, Enum e) => Queue bits e -> Queue bits e -> Queue bits e
difference q0@(Queue ns0 m0) (Queue ns1 m1) =
   Queue
      (BitMap.sub ns0 $ BitMap.intersectionSet ns1 $ keysBits q0)
      (IntMap.differenceWith ((Just.) . EnumSet.difference) m0 m1)

partition ::
   (BitPos.C bits, Enum e) =>
   Queue bits e -> BitSet.Set bits -> (Queue bits e, Queue bits e)
partition (Queue ns m) s =
   let section = IntMap.intersection m $ constIntMapFromBits () s
   in  (Queue (BitMap.intersectionSet ns s) section,
        Queue (BitMap.differenceSet ns s) $ IntMap.difference m section)
