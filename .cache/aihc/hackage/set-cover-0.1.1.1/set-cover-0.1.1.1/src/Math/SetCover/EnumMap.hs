module Math.SetCover.EnumMap where

import qualified Math.SetCover.BitPosition as BitPos
import qualified Math.SetCover.BitSet as BitSet

import qualified Data.EnumMap as EnumMap; import Data.EnumMap (EnumMap)
import qualified Data.EnumSet as EnumSet; import Data.EnumSet (EnumSet)
import qualified Data.IntMap as IntMap; import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet; import Data.IntSet (IntSet)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (const)


-- EnumMap.fromSet is available from containers-0.5
const :: (Enum e) => a -> EnumSet e -> EnumMap e a
const a = EnumMap.fromAscList . map (\k -> (k, a)) . EnumSet.toAscList

intersection :: (Enum e) => EnumMap e a -> EnumSet e -> EnumMap e a
intersection m s = EnumMap.intersection m $ const () s

partition ::
   (Enum e) => EnumMap e a -> EnumSet e -> (EnumMap e a, EnumMap e a)
partition m s =
   let section = intersection m s
   in  (section, EnumMap.difference m section)


{-# INLINE attach #-}
attach :: b -> [a] -> [(a, b)]
attach a = map (flip (,) a)

-- Map.fromSet is available from containers-0.5
constMap :: (Ord a) => b -> Set.Set a -> Map.Map a b
constMap a = Map.fromAscList . attach a . Set.toAscList

transposeSet ::
   (Enum e, Ord a) => EnumMap e (Set.Set a) -> Map.Map a (EnumSet e)
transposeSet =
   Map.unionsWith EnumSet.union . EnumMap.elems .
   EnumMap.mapWithKey (constMap . EnumSet.singleton)


transposeMap ::
   (Enum e, Ord a) => EnumMap e (Map.Map a b) -> Map.Map a (EnumMap e b)
transposeMap =
   Map.unionsWith EnumMap.union . EnumMap.elems .
   EnumMap.mapWithKey (fmap . EnumMap.singleton)


constIntMapFromBits :: (BitPos.C bits) => b -> BitSet.Set bits -> IntMap b
constIntMapFromBits a = IntMap.fromAscList . attach a . BitPos.unpack

transposeBitSet ::
   (BitPos.C bits, Enum e) => EnumMap e (BitSet.Set bits) -> IntMap (EnumSet e)
transposeBitSet =
   IntMap.unionsWith EnumSet.union . EnumMap.elems .
   EnumMap.mapWithKey (constIntMapFromBits . EnumSet.singleton)


constIntMap :: b -> IntSet -> IntMap b
constIntMap a = IntMap.fromAscList . attach a . IntSet.toAscList

transposeIntSet :: (Enum e) => EnumMap e IntSet -> IntMap (EnumSet e)
transposeIntSet =
   IntMap.unionsWith EnumSet.union . EnumMap.elems .
   EnumMap.mapWithKey (constIntMap . EnumSet.singleton)
