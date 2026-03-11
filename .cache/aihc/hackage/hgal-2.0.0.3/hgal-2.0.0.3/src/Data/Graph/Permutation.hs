{-| Module      :  Data.Graph.Permutation
    Copyright   :  (c) Jean-Philippe Bernardy 2003
    License     :  GPL

    Maintainer  :  JeanPhilippe.Bernardy@gmail.com
    Stability   :  proposal
    Portability :  GHC


This modules manages permutations between nodes of a graph. Permutations are represented as arrays.

-}


module Data.Graph.Permutation (Permutation, fixed, permBetween, applyPerm, orbitsFromPerm, mergePerms) where

import Data.Array
import Data.List
import Data.Graph
import Data.Graph.Partition
import Data.Tree (flatten)

-- | A permutations maps a range of Vertices to itself.
type Permutation = Array Vertex Vertex

-- | Fixed vertices of a given permutation
fixed :: Permutation -> [Vertex]
fixed perm = [i | i <- range $ bounds perm, perm!i == i]


-- | Builds the permutation taking l1 on l2.
permBetween :: Bounds -> [Vertex] -> [Vertex] -> Permutation
permBetween bnds l1 l2 = array bnds (zip l1 l2)

-- | Relabel a graph using a permutation
applyPerm :: Permutation -> Graph -> Graph
applyPerm perm gr = array bnds [(perm!x, sort $ map (perm!) (gr!x)) | x <- range bnds]
    where bnds = bounds gr


-- | Returns the graph of the permutation
permAsGraph :: Permutation -> Graph
permAsGraph = fmap return

-- | Returns the orbits of a permutation, as a partition
orbitsFromPerm :: Permutation -> Partition
orbitsFromPerm = map flatten . dff . permAsGraph

-- | Returns a permutation whose orbits are given.
permFromOrbits :: Bounds -> Partition -> Permutation
permFromOrbits bnds orbits = array bnds $ concatMap cycleOf orbits
    where cycleOf' first (v1:v2:vs) = (v1, v2) : cycleOf' first (v2:vs)
          cycleOf' first [v] = [(v, first)]
          cycleOf' _ _ = []
          cycleOf orbit@(v:_) = cycleOf' v orbit
          cycleOf _ = []

-- | Merge the orbits of two permutations
mergePerms :: Permutation -> Permutation -> Permutation
mergePerms p1 p2 = permFromOrbits (bounds p1) $
                   map flatten $
                   dff $
                   listArray (bounds p1) (zipWith (\v1 v2->[v1, v2]) (elems p1) (elems p2))
