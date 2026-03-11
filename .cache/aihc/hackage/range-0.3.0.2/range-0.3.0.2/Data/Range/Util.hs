{-# LANGUAGE Safe #-}

module Data.Range.Util where

import Data.Maybe (catMaybes)

import Data.Range.Data

-- This module is supposed to contain all of the functions that are required by the rest
-- of the code but could be easily pulled into separate and completely non-related
-- codebases or libraries.

compareLower :: Ord a => Bound a -> Bound a -> Ordering
compareLower ab@(Bound a aType) bb@(Bound b _)
   | ab == bb     = EQ
   | a == b       = if aType == Inclusive then LT else GT
   | a < b        = LT
   | otherwise    = GT

compareHigher :: Ord a => Bound a -> Bound a -> Ordering
compareHigher ab@(Bound a aType) bb@(Bound b _)
   | ab == bb     = EQ
   | a == b       = if aType == Inclusive then GT else LT
   | a < b        = LT
   | otherwise    = GT

compareLowerIntersection :: Ord a => Bound a -> Bound a -> Ordering
compareLowerIntersection ab@(Bound a aType) bb@(Bound b _)
   | ab == bb     = EQ
   | a == b       = if aType == Exclusive then LT else GT
   | a < b        = LT
   | otherwise    = GT

compareHigherIntersection :: Ord a => Bound a -> Bound a -> Ordering
compareHigherIntersection ab@(Bound a aType) bb@(Bound b _)
   | ab == bb     = EQ
   | a == b       = if aType == Exclusive then GT else LT
   | a < b        = LT
   | otherwise    = GT

compareUpperToLower :: Ord a => Bound a -> Bound a -> Ordering
compareUpperToLower (Bound upper upperType) (Bound lower lowerType)
   | upper == lower  = if upperType == Inclusive || lowerType == Inclusive then EQ else LT
   | upper < lower   = LT
   | otherwise       = GT

minBounds :: Ord a => Bound a -> Bound a -> Bound a
minBounds ao bo = if compareLower ao bo == LT then ao else bo

maxBounds :: Ord a => Bound a -> Bound a -> Bound a
maxBounds ao bo = if compareHigher ao bo == GT then ao else bo

minBoundsIntersection :: Ord a => Bound a -> Bound a -> Bound a
minBoundsIntersection ao bo = if compareLowerIntersection ao bo == LT then ao else bo

maxBoundsIntersection :: Ord a => Bound a -> Bound a -> Bound a
maxBoundsIntersection ao bo = if compareHigherIntersection ao bo == GT then ao else bo

insertionSort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertionSort comp xs ys = go xs ys
   where
      go (f : fs) (s : ss) = case comp f s of
         LT -> f : go fs (s : ss)
         EQ -> f : s : go fs ss
         GT -> s : go (f : fs) ss
      go [] z = z
      go z [] = z

invertBound :: Bound a -> Bound a
invertBound (Bound x Inclusive) = Bound x Exclusive
invertBound (Bound x Exclusive) = Bound x Inclusive

isEmptySpan :: Eq a => (Bound a, Bound a) -> Bool
isEmptySpan (Bound a aType, Bound b bType) = a == b && (aType == Exclusive || bType == Exclusive)

removeEmptySpans :: Eq a => [(Bound a, Bound a)] -> [(Bound a, Bound a)]
removeEmptySpans = filter (not . isEmptySpan)

boundsOverlapType :: Ord a => (Bound a, Bound a) -> (Bound a, Bound a) -> OverlapType
boundsOverlapType l@(ab@(Bound a _), bb@(Bound b _)) r@(xb@(Bound x _), yb@(Bound y _))
   | isEmptySpan l || isEmptySpan r    = Separate
   | a == x                            = Overlap
   | b == y                            = Overlap
   | otherwise = (ab `boundIsBetween` (xb, yb)) `orOverlapType` (xb `boundIsBetween` (ab, bb))

orOverlapType :: OverlapType -> OverlapType -> OverlapType
orOverlapType Overlap _ = Overlap
orOverlapType _ Overlap = Overlap
orOverlapType Adjoin _ = Adjoin
orOverlapType _ Adjoin = Adjoin
orOverlapType _ _ = Separate

pointJoinType :: BoundType -> BoundType -> OverlapType
pointJoinType Inclusive Inclusive = Overlap
pointJoinType Exclusive Exclusive = Separate
pointJoinType _ _ = Adjoin

-- This function assumes that the bound on the left is a lower bound and that the range is in (lower, upper)
-- bound order
boundCmp :: (Ord a) => Bound a -> (Bound a, Bound a) -> Ordering
boundCmp ab@(Bound a _) (xb@(Bound x _), yb)
   | boundIsBetween ab (xb, yb) /= Separate = EQ
   | a <= x = LT
   | otherwise = GT

-- TODO replace everywhere with boundsOverlapType
boundIsBetween :: (Ord a) => Bound a -> (Bound a, Bound a) -> OverlapType
boundIsBetween (Bound a aType) (Bound x xType, Bound y yType)
   | x > a     = Separate
   | x == a    = pointJoinType aType xType
   | a < y     = Overlap
   | a == y    = pointJoinType aType yType
   | otherwise = Separate

singletonInSpan :: Ord a => a -> (Bound a, Bound a) -> OverlapType
singletonInSpan a span' = boundIsBetween (Bound a Inclusive) span'

againstLowerBound :: Ord a => Bound a -> Bound a -> OverlapType
againstLowerBound (Bound a aType) (Bound lower lowerType)
   | lower == a   = pointJoinType aType lowerType
   | lower < a    = Overlap
   | otherwise    = Separate

againstUpperBound :: Ord a => Bound a -> Bound a -> OverlapType
againstUpperBound (Bound a aType) (Bound upper upperType)
   | upper == a   = pointJoinType aType upperType
   | a < upper    = Overlap
   | otherwise    = Separate

takeEvenly :: [[a]] -> [a]
takeEvenly [] = []
takeEvenly xss = (catMaybes . map safeHead $ xss) ++ takeEvenly (filter (not . null) . map tail $ xss)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

lowestValueInLowerBound :: Enum a => Bound a -> a
lowestValueInLowerBound (Bound a Inclusive) = a
lowestValueInLowerBound (Bound a Exclusive) = succ a

highestValueInUpperBound :: Enum a => Bound a -> a
highestValueInUpperBound (Bound a Inclusive) = a
highestValueInUpperBound (Bound a Exclusive) = pred a