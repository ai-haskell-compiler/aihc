-- |
-- Module      :  ELynx.Tree.Partition
-- Description :  Partitions on rose trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Dec 12 12:58:49 2019.
module ELynx.Tree.Partition
  ( -- * Data type
    Partition (fromPartition),
    pt,
    ptUnsafe,
    bpToPt,
    ptHuman,

    -- * Work with 'Partition's
    partition,
    partitions,
    compatible,
  )
where

import Data.List hiding (partition)
import Data.Set (Set)
import qualified Data.Set as S
import ELynx.Tree.Bipartition
import ELynx.Tree.Rooted

-- | A partition of a tree is a grouping of the leaves of the tree into
-- non-overlapping, non-empty sub sets.
--
-- For unrooted trees:
--
-- - For example, each branch of an unrooted tree partitions the leaves of the
--   tree into two subsets (see 'ELynx.Tree.Bipartition').
--
-- For rooted trees:
--
-- - In a similar way, each bifurcating internal node (excluding the root node)
--   partitions the leaves into three subsets called a 'Partition'. If the tree
--   is multifurcating, and a specific node has more than two children, the
--   number of subsets induced by this node is larger than three. Partitions are
--   interesting in that we can use them for calculating incompatible splits,
--   see 'ELynx.Tree.Distance'.
--
-- The order of the subsets of a 'Partition' is meaningless. We ensure by
-- construction that the subsets are ordered, and hence, that equality checks
-- are meaningful.
newtype Partition a = Partition
  { fromPartition :: Set (Set a)
  }
  deriving (Eq, Ord, Show, Read)

-- | Create a partition.
pt :: Ord a => [Set a] -> Either String (Partition a)
pt xs = case filter (not . S.null) xs of
  [] -> Left "pt: Empty list."
  xs' -> Right $ ptUnsafe xs'

-- | Create a partition.
ptUnsafe :: Ord a => [Set a] -> Partition a
ptUnsafe xs = Partition (S.fromList xs)

-- | Convert a bipartition to a partition.
bpToPt :: Ord a => Bipartition a -> Partition a
bpToPt = ptUnsafe . tupleToList . fromBipartition
  where
    -- Be careful with tuples, because 'toList' does something very weird. It only
    -- takes the second element of the tuple!
    --
    -- toList :: Foldable t => t a -> [a]
    tupleToList (x, y) = [x, y]

-- | Show a partition in a human readable form. Use a provided function to
-- extract the valuable information.
ptHuman :: Show a => Partition a -> String
ptHuman (Partition xs) =
  "(" ++ intercalate "|" (map setShow (S.toList xs)) ++ ")"

-- Show the elements of a set in a human readable format.
setShow :: Show a => Set a -> String
setShow = intercalate "," . map show . S.toList

-- | Get partition defined by the root of the tree.
--
-- Return 'Left' if:
-- - the tree is a leaf;
-- - the tree contains duplicate leaves.
partition :: Ord a => Tree e a -> Either String (Partition a)
partition (Node _ _ []) = Left "partition: Encountered a leaf."
partition t@(Node _ _ ts)
  | duplicateLeaves t = Left "partition: Tree contains duplicate leaves."
  | otherwise = pt $ map (S.fromList . leaves) ts

-- | Get all 'Partition's of a tree.
--
-- Return 'Left' if tree contains duplicate leaves.
partitions :: Ord a => Tree e a -> Either String (Set (Partition a))
partitions t
  | duplicateLeaves t = Left "partitions: Tree contains duplicate leaves."
  | otherwise = Right $ partitions' S.empty $ S.fromList <$> groups t

-- See 'partitions', but do not check if leaves are unique.
partitions' :: Ord a => Set a -> Tree e (Set a) -> Set (Partition a)
partitions' _ (Node _ _ []) = S.empty
partitions' p t@(Node _ _ ts) =
  S.unions $
    either (const S.empty) S.singleton (pt (p : map label ts))
      : zipWith partitions' cs ts
  where
    cs = getComplementaryLeaves p t

-- | 'Partition's are compatible if they do not contain conflicting
-- information. This function checks if two partitions are compatible with
-- each other. Thereby, a variation of the following algorithm is used:
--
-- @
-- mp1 `compatible` mp2
-- for set1 in mp1:
--   for set2 in mp2:
--     if set1 `S.isSubSetOf` set2:
--       remove set1 from mp1
--     if set2 `S.isSubSetOf` set1:
--       remove set2 from mp2
-- if either mp2 or mp2 is empty, they are compatible
-- @
compatible :: (Show a, Ord a) => Partition a -> Partition a -> Bool
compatible l r = S.null (S.filter (`remove` rs) ls) || S.null (S.filter (`remove` ls) rs)
  where
    ls = fromPartition l
    rs = fromPartition r

remove :: Ord a => Set a -> Set (Set a) -> Bool
remove s = not . any (s `S.isSubsetOf`)
