{-| Module      :  Data.Graph.Partition
    Copyright   :  (c) Jean-Philippe Bernardy 2003
    License     :  GPL

    Maintainer  :  JeanPhilippe.Bernardy@gmail.com
    Stability   :  proposal
    Portability :  GHC


    Implementation of equitable partitioning of graphs + indicator function.

    The implementation is based on:
    Brendan D. McKay, PRACTICAL GRAPH ISOMORPHISM,
    in Congressus Numerantium,
    Vol. 30 (1981), pp. 45-87.

-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}
module Data.Graph.Partition(Cell, Partition, refine, isSingleton,
                            unitPartition, isDiscrete, mcr,
                            Indicator, lambda, lambda_, fixedInOrbits) where

import Data.Graph
import Data.List
import Data.Array((!), range, bounds)
import Data.Int
import Data.Bits
import qualified Data.Map as Map

-- | A cell is represented by its list of vertices,
-- with the invariant that the list is sorted
type Cell = [Vertex]

-- | A partition is its list of cells
type Partition = [Cell]

-- Tells whether a list has a single element.
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

-- | The unit partition of a range.
unitPartition :: (Vertex, Vertex) -> Partition
unitPartition bnds = [range bnds]

-- | Is the partition discrete ?
isDiscrete :: Partition -> Bool
isDiscrete = all isSingleton

-- | Refines a Partition wrt to another Partition, given a graph.
-- (explained on pages 50-52)
-- This is equivalent to partition the graph's DFA in equivalent states.
-- @refine gr p q@ refines @p@ wrt. @q@ in @gr@.
refine :: Graph -> Partition -> Partition -> Partition
refine _  p [] = p
refine gr p (w:ws) = refine gr p' alpha
  where (p', alpha) = refineCells p ws

        refineCells [] q = ([], q)
        refineCells (c:cs) q = (rc ++ rcs, xxq)
          where (rc, xq) = refineCell c q
                (rcs, xxq) = refineCells cs xq

        refineCell :: Cell -> [Cell] -> (Partition, [Cell])
        refineCell [v] alph = ([[v]], alph)
        refineCell c alph
         | isSingleton xs = ([c], alph)
         | otherwise = (xs, alph' ++ smallXs)
          where
                xs = refineCellByOneCell c w
                alph' = replace (c ==) largeXt alph
                (largeXt, smallXs) = extractLargest xs

        -- splits a cell in groups of equal degrees with respect to another cell.
        refineCellByOneCell :: Cell -> Cell -> Partition
        refineCellByOneCell refinedCell referenceCell =
          groupSortBy (degreeCellVertex gr referenceCell) refinedCell

replace :: (a->Bool) -> a -> [a] -> [a]
replace _ _   [] = []
replace f rep (l:ls)
  | f l = rep:ls
  | otherwise = l:replace f rep ls

-- TODO: try if the below is faster.
-- replace f a = map (\x -> if f x then a else x)

extractLargest :: [[a]] -> ([a], [[a]])
extractLargest list = (largest, before ++ after)
  where (before, largest:after) = break hasMaxLength list
        hasMaxLength el = length el == maxLength
        maxLength = maximum $ map length list

groupSortBy :: Ord k => (a -> k) -> [a] -> [[a]]

--groupSortBy key list = map (map snd) $ groupBy fstEq $ sortBy fstComp $ [(key v, v) | v <- list]
--    where fstComp x y = compare (fst x) (fst y)
--        fstEq x y = fst x == fst y

groupSortBy f list = map snd $ Map.toList $ Map.fromListWith (++) [(f v, [v]) | v <- list]
-- TODO: for some reason replacing map snd $ Map.toList by Map.elems makes the program slower. Investigate.

mcr :: Partition -> [Vertex]
mcr = map head

-- | Returns vertices fixes in the given orbits
fixedInOrbits :: Partition -> [Vertex]
fixedInOrbits part = map head $ filter isSingleton part

isNeighbour :: Graph -> Vertex -> Vertex -> Bool
isNeighbour gr n1 n2 = n2 `elem` (gr!n1)

-- TODO: try to keep graph sorted and use the below instead of elem.
-- elemInSorted :: Ord a => a -> [a] -> Bool
-- elemInSorted _ [] = False
-- elemInSorted y (h:t) = case compare y h of
--                          LT -> elemInSorted y t
--                          EQ -> True
--                          GT -> False

-- | degree of a cell wrt a node
degreeCellVertex :: Graph -> Cell -> Vertex -> Int
degreeCellVertex gr cell vertex = count (isNeighbour gr vertex) cell
    where count p = foldr (\v->if p v then (+1) else id) 0




----------------------------------------
-- The indicator function


type Indicator = Int32

-- | An order-insensitive hash
oih :: [Indicator] -> Indicator
oih = foldr xor 0

-- | An order-sensitive hash
osh :: [Indicator] -> Indicator
osh = foldl' (\x y -> 97 * y + x + 1230497) 1

-- | An indicator function.
-- @lambda@ must be insensitive to automorphisms relabeling of the graph for the Automorphism module to work.
lambda :: Graph -> Partition -> Indicator
lambda gr nu
    = osh [oih $ map (fromIntegral . degreeCellVertex gr c) (range $ bounds gr) | c <- nu]

-- prop_lambda gr pi gamma = lambda gr pi == lambda (applyPerm gamma gr) (applyPermPart gamma pi)
--  where gamma is an automorphism of gr

lambda_ :: Graph -> [Partition] -> [Indicator]
lambda_ gr = map (lambda gr)
