{-# LANGUAGE Safe #-}

-- This module contains every function that purely performs operations on spans.
module Data.Range.Spans where

import Data.Range.Util
import Data.Range.Data

-- Assume that both inputs are sorted spans
insertionSortSpans :: (Ord a) => [(Bound a, Bound a)] -> [(Bound a, Bound a)] -> [(Bound a, Bound a)]
insertionSortSpans = insertionSort (\a b -> compareLower (fst a) (fst b))

spanCmp :: Ord a => (Bound a, Bound a) -> (Bound a, Bound a) -> Ordering
spanCmp x@(_, Bound xHighValue _) y@(Bound yLowValue _, _) =
   if boundsOverlapType x y /= Separate
      then EQ
      else if xHighValue <= yLowValue then LT else GT

intersectSpans :: (Ord a) => [(Bound a, Bound a)] -> [(Bound a, Bound a)] -> [(Bound a, Bound a)]
intersectSpans (x@(xlow, xup@(Bound xUpValue _)) : xs) (y@(ylow, yup@(Bound yUpValue _)) : ys) =
   case spanCmp x y of
      EQ -> if (not . isEmptySpan $ intersectedSpan) then intersectedSpan : equalNext else equalNext
      LT -> intersectSpans xs (y : ys)
      GT -> intersectSpans (x : xs) ys
   where
      intersectedSpan = (maxBoundsIntersection xlow ylow, minBoundsIntersection xup yup)

      lessThanNext = intersectSpans xs (y : ys)
      greaterThanNext = intersectSpans (x : xs) ys
      equalNext = if xUpValue < yUpValue then lessThanNext else greaterThanNext

intersectSpans _ _ = []


-- Assume that you are given a sorted list of spans
joinSpans :: (Eq a, Enum a) => [(Bound a, Bound a)] -> [(Bound a, Bound a)]
joinSpans (f@(a, b) : s@(x, y) : xs) =
   if (succ . highestValueInUpperBound $ b) == lowestValueInLowerBound x
      then joinSpans $ (a, y) : xs
      else f : joinSpans (s : xs)
joinSpans xs = xs

-- Assume that you are given a sorted list of spans
unionSpans :: Ord a => [(Bound a, Bound a)] -> [(Bound a, Bound a)]
unionSpans (f@(a, b) : s@(_, y) : xs) = if boundsOverlapType f s /= Separate
   then unionSpans ((a, maxBounds b y) : xs)
   else f : unionSpans (s : xs)
unionSpans xs = xs

-- Assume that you are given a sorted and joined list of spans
invertSpans :: [(Bound a, Bound a)] -> [(Bound a, Bound a)]
invertSpans ((_, x) : s@(y, _) : xs) = (invertBound x, invertBound y) : invertSpans (s : xs)
invertSpans _ = []
