-- |
-- Module      :  ELynx.Topology.Phylogeny
-- Description :  Phylogenetic topologies
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 18 13:15:49 2020.
--
-- A topology, as it is used in phylogenetics is a 'Topology' with unique leaf
-- labels, and the order of the topologies in the sub-forest is considered to be
-- meaningless.
--
-- NOTE: The functions in this module are defined using the functions in
-- "ELynx.Tree.Phylogeny". This induces a runtime overhead, but greatly reduces
-- the probability of additional bugs.
module ELynx.Topology.Phylogeny
  ( equal,
    equal',
    intersect,
    bifurcating,
    outgroup,
    midpoint,
    roots,
  )
where

import Data.Default.Class
import Data.List hiding (intersect)
import Data.Maybe
import qualified Data.Set as S
import ELynx.Topology.Rooted
import ELynx.Tree.Length
import qualified ELynx.Tree.Phylogeny as T
import qualified ELynx.Tree.Rooted as T

-- | The equality check is slow because the order of children is considered to
-- be arbitrary.
--
-- Return 'Left' if a topology does not have unique leaves.
equal :: (Eq a, Ord a) => Topology a -> Topology a -> Either String Bool
equal tL tR
  | duplicateLeaves tL = Left "equal: Left topology has duplicate leaves."
  | duplicateLeaves tR = Left "equal: Right topology has duplicate leaves."
  | otherwise = Right $ equal' tL tR

-- | Same as 'equal', but assume that leaves are unique.
equal' :: Eq a => Topology a -> Topology a -> Bool
equal' (Leaf lbL) (Leaf lbR) =
  lbL == lbR
equal' (Node tsL) (Node tsR) =
  (length tsL == length tsR)
    && all (`elem'` tsR) tsL
  where
    elem' t ts = isJust $ find (equal' t) ts
equal' _ _ = False

-- | Intersection of topologies.
--
-- | See 'ELynx.Tree.Phylogeny.intersect'.
intersect ::
  (Ord a) => [Topology a] -> Either String [Topology a]
intersect ts
  | S.null lvsCommon = Left "intersect: Intersection of leaves is empty."
  | otherwise = case sequence [dropLeavesWith (predicate ls) t | (ls, t) <- zip leavesToDrop ts] of
      Nothing -> Left "intersect: A topology is empty."
      Just ts' -> Right ts'
  where
    -- Leaf sets.
    lvss = map (S.fromList . leaves) ts
    -- Common leaf set.
    lvsCommon = foldl1' S.intersection lvss
    -- Leaves to drop for each topology in the forest.
    leavesToDrop = map (S.\\ lvsCommon) lvss
    -- Predicate.
    predicate lvsToDr l = l `S.member` lvsToDr

-- | Check if topology is bifurcating.
--
-- | See 'ELynx.Tree.Phylogeny.intersect'.
bifurcating :: Topology a -> Bool
bifurcating (Leaf _) = True
bifurcating (Node ts) = (length ts == 2) && all bifurcating ts

-- Perform a computation over the 'Tree' data type.
overTree ::
  (Default a, Functor f) =>
  (T.Tree Length a -> f (T.Tree Length a)) ->
  Topology a ->
  f (Topology a)
overTree f = goBack . f . goThere
  where
    goThere = toBranchLabelTreeWith (toLengthUnsafe 1.0) def
    goBack = fmap fromBranchLabelTree

-- | Root topology using an outgroup.
--
--   See 'ELynx.Tree.Phylogeny.outgroup'.
outgroup :: (Default a, Ord a) => S.Set a -> Topology a -> Either String (Topology a)
outgroup xs = overTree (T.outgroup xs)

-- | Root topology at the midpoint.
--
-- See 'ELynx.Tree.Phylogeny.midpoint'.
--
-- Use 'depth' to measure topology height.
--
-- If the midpoint is ambiguous because the sum of the left and right depths is
-- odd, the depth of the left sub-topology will be set to be one node greater
-- than the one of the right sub-topology.
midpoint :: Default a => Topology a -> Either String (Topology a)
midpoint = overTree T.midpoint

-- | For a rooted tree with a bifurcating root node, get all possible rooted
-- trees.
--
-- See 'ELynx.Tree.Phylogeny.roots'.
roots :: Default a => Topology a -> Either String [Topology a]
roots = goBack . T.roots . goThere
  where
    -- We have to use a special 'overTree' function here, since a list of
    -- topologies is returned.
    goThere = toBranchLabelTreeWith (toLengthUnsafe 1.0) def
    goBack = (fmap . fmap) fromBranchLabelTree
