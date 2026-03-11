{-# LANGUAGE Safe #-}

module Data.Range.RangeInternal where

import Data.Maybe (catMaybes)

import Data.Range.Data
import Data.Range.Spans
import Data.Range.Util

import Control.Monad (guard)

{-
 - The following assumptions must be maintained at the beginning of these internal
 - functions so that we can reason about what we are given.
 -
 - RangeMerge assumptions:
 - * The span ranges will never overlap the bounds.
 - * The span ranges are always sorted in ascending order by the first element.
 - * The lower and upper bounds never overlap in such a way to make it an infinite range.
 -}
data RangeMerge a = RM
   { largestLowerBound :: Maybe (Bound a)
   , largestUpperBound :: Maybe (Bound a)
   , spanRanges :: [(Bound a, Bound a)]
   }
   | IRM
   deriving (Show, Eq)

emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM Nothing Nothing []

storeRange :: (Ord a) => Range a -> RangeMerge a
storeRange InfiniteRange = IRM
storeRange (LowerBoundRange lower) = emptyRangeMerge { largestLowerBound = Just lower }
storeRange (UpperBoundRange upper) = emptyRangeMerge { largestUpperBound = Just upper }
storeRange (SpanRange x@(Bound xValue xType) y@(Bound yValue yType))
   | xValue == yValue && pointJoinType xType yType == Separate = emptyRangeMerge
   | otherwise = emptyRangeMerge { spanRanges = [(minBounds x y, maxBounds x y)] }
storeRange (SingletonRange x) = emptyRangeMerge { spanRanges = [(Bound x Inclusive, Bound x Inclusive)] }

storeRanges :: (Ord a) => RangeMerge a -> [Range a] -> RangeMerge a
storeRanges start ranges = foldr unionRangeMerges start (map storeRange ranges)

loadRanges :: (Ord a) => [Range a] -> RangeMerge a
loadRanges = storeRanges emptyRangeMerge
{-# INLINE[0] loadRanges #-}

exportRangeMerge :: (Eq a) => RangeMerge a -> [Range a]
exportRangeMerge IRM = [InfiniteRange]
exportRangeMerge (RM lb up spans) = putUpperBound up ++ putSpans spans ++ putLowerBound lb
   where
      putLowerBound :: Maybe (Bound a) -> [Range a]
      putLowerBound = maybe [] (return . LowerBoundRange)
      putUpperBound :: Maybe (Bound a) -> [Range a]
      putUpperBound = maybe [] (return . UpperBoundRange)
      putSpans = map simplifySpan

      simplifySpan (x@(Bound xv xType), y@(Bound _ yType)) = if (x == y) && (pointJoinType xType yType /= Separate)
         then SingletonRange xv
         else SpanRange x y

{-# RULES "load/export" [1] forall x. loadRanges (exportRangeMerge x) = x #-}

intersectSpansRM :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectSpansRM one two = RM Nothing Nothing newSpans
   where
      newSpans = intersectSpans (spanRanges one) (spanRanges two)

intersectWith :: (Ord a) => (Bound a -> (Bound a, Bound a) -> Maybe (Bound a, Bound a)) -> Maybe (Bound a) -> [(Bound a, Bound a)] -> [(Bound a, Bound a)]
intersectWith _ Nothing _ = []
intersectWith fix (Just lower) xs = catMaybes $ fmap (fix lower) xs

fixLower :: (Ord a) => Bound a -> (Bound a, Bound a) -> Maybe (Bound a, Bound a)
fixLower lower@(Bound lowerValue _) (x, y@(Bound yValue _)) = do
   guard (lowerValue <= yValue)
   return (maxBoundsIntersection lower x, y)

fixUpper :: (Ord a) => Bound a -> (Bound a, Bound a) -> Maybe (Bound a, Bound a)
fixUpper upper@(Bound upperValue _) (x@(Bound xValue _), y) = do
   guard (xValue <= upperValue)
   return (x, minBoundsIntersection y upper)

intersectionRangeMerges :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectionRangeMerges IRM two = two
intersectionRangeMerges one IRM = one
intersectionRangeMerges one two = RM
   { largestLowerBound = newLowerBound
   , largestUpperBound = newUpperBound
   , spanRanges = unionSpans sortedResults
   }
   where
      lowerOneSpans = intersectWith fixLower (largestLowerBound one) (spanRanges two)
      lowerTwoSpans = intersectWith fixLower (largestLowerBound two) (spanRanges one)
      upperOneSpans = intersectWith fixUpper (largestUpperBound one) (spanRanges two)
      upperTwoSpans = intersectWith fixUpper (largestUpperBound two) (spanRanges one)
      intersectedSpans = intersectSpans (spanRanges one) (spanRanges two)

      sortedResults = removeEmptySpans $ foldr1 insertionSortSpans
         [ lowerOneSpans
         , lowerTwoSpans
         , upperOneSpans
         , upperTwoSpans
         , intersectedSpans
         , calculateBoundOverlap one two
         ]

      newLowerBound = calculateNewBound largestLowerBound maxBoundsIntersection one two
      newUpperBound = calculateNewBound largestUpperBound minBoundsIntersection one two

      calculateNewBound
         :: (Ord a)
         => (RangeMerge a -> Maybe (Bound a))
         -> (Bound a -> Bound a -> Bound a)
         -> RangeMerge a -> RangeMerge a -> Maybe (Bound a)
      calculateNewBound ext comp one' two' = case (ext one', ext two') of
         (Just x, Just y) -> Just $ comp x y
         (_, Nothing) -> Nothing
         (Nothing, _) -> Nothing

calculateBoundOverlap :: (Ord a) => RangeMerge a -> RangeMerge a -> [(Bound a, Bound a)]
calculateBoundOverlap one two = catMaybes [oneWay, secondWay]
   where
      oneWay = do
         x <- largestLowerBound one
         y <- largestUpperBound two
         guard (compareLower y x /= LT)
         return (x, y)

      secondWay = do
         x <- largestLowerBound two
         y <- largestUpperBound one
         guard (compareLower y x /= LT)
         return (x, y)

unionRangeMerges :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
unionRangeMerges IRM _ = IRM
unionRangeMerges _ IRM = IRM
unionRangeMerges one two = infiniteCheck filterTwo
   where
      filterOne = foldr filterLowerBound boundedRM (unionSpans sortedSpans)
      filterTwo = foldr filterUpperBound (filterOne { spanRanges = [] }) (spanRanges filterOne)

      infiniteCheck :: (Ord a) => RangeMerge a -> RangeMerge a
      infiniteCheck IRM = IRM
      infiniteCheck rm@(RM (Just lower) (Just upper) _) = if compareUpperToLower upper lower /= LT
         then IRM
         else rm
      infiniteCheck rm = rm

      newLowerBound = calculateNewBound largestLowerBound minBounds one two
      newUpperBound = calculateNewBound largestUpperBound maxBounds one two

      sortedSpans = insertionSortSpans (spanRanges one) (spanRanges two)

      boundedRM = RM
         { largestLowerBound = newLowerBound
         , largestUpperBound = newUpperBound
         , spanRanges = []
         }

      calculateNewBound
         :: (Ord a)
         => (RangeMerge a -> Maybe (Bound a))
         -> (Bound a -> Bound a -> Bound a)
         -> RangeMerge a -> RangeMerge a -> Maybe (Bound a)
      calculateNewBound ext comp one' two' = case (ext one', ext two') of
         (Just x, Just y) -> Just $ comp x y
         (z, Nothing) -> z
         (Nothing, z) -> z

filterLowerBound :: (Ord a) => (Bound a, Bound a) -> RangeMerge a -> RangeMerge a
filterLowerBound _ IRM = IRM
filterLowerBound a rm@(RM Nothing _ _) = rm { spanRanges = a : spanRanges rm }
filterLowerBound s@(lower, _) rm@(RM (Just lowestBound) _ _) =
   case boundCmp lowestBound s of
      GT -> rm { spanRanges = s : spanRanges rm }
      LT -> rm
      EQ -> rm { largestLowerBound = Just $ minBounds lowestBound lower }

filterUpperBound :: (Ord a) => (Bound a, Bound a) -> RangeMerge a -> RangeMerge a
filterUpperBound _ IRM = IRM
filterUpperBound a rm@(RM _ Nothing _) = rm { spanRanges = a : spanRanges rm }
filterUpperBound s@(_, upper) rm@(RM _ (Just upperBound) _) =
   case boundCmp upperBound s of
      LT -> rm { spanRanges = s : spanRanges rm }
      GT -> rm
      EQ -> rm { largestUpperBound = Just $ maxBounds upperBound upper }

invertRM :: (Ord a) => RangeMerge a -> RangeMerge a
invertRM IRM = emptyRangeMerge
invertRM (RM Nothing Nothing []) = IRM
invertRM (RM (Just lower) Nothing []) = RM Nothing (Just . invertBound $ lower) []
invertRM (RM Nothing (Just upper) []) = RM (Just . invertBound $ upper) Nothing []
invertRM (RM (Just lower) (Just upper) []) = RM Nothing Nothing [(invertBound upper, invertBound lower)]
invertRM rm = RM
   { largestUpperBound = newUpperBound
   , largestLowerBound = newLowerBound
   , spanRanges = upperSpan ++ betweenSpans ++ lowerSpan
   }
   where
      newLowerValue = invertBound . snd . last . spanRanges $ rm
      newUpperValue = invertBound . fst . head . spanRanges $ rm

      newUpperBound = case largestUpperBound rm of
         Just _ -> Nothing
         Nothing -> Just newUpperValue

      newLowerBound = case largestLowerBound rm of
         Just _ -> Nothing
         Nothing -> Just newLowerValue

      upperSpan = case largestUpperBound rm of
         Nothing -> []
         Just upper -> [(invertBound upper, newUpperValue)]
      lowerSpan = case largestLowerBound rm of
         Nothing -> []
         Just lower -> [(newLowerValue, invertBound lower)]

      betweenSpans = invertSpans . spanRanges $ rm

joinRM :: (Eq a, Enum a) => RangeMerge a -> RangeMerge a
joinRM o@(RM _ _ []) = o
joinRM rm = RM lower higher spansAfterHigher
   where
      joinedSpans = joinSpans . spanRanges $ rm

      (lower, spansAfterLower) =
         case (largestLowerBound rm, reverse joinedSpans) of
            o@(Just l, ((xl, xh) : xs)) ->
               if (succ . highestValueInUpperBound $ xh) == lowestValueInLowerBound l
                  then (Just xl, reverse xs)
                  else o
            x -> x

      (higher, spansAfterHigher) =
         case (largestUpperBound rm, spansAfterLower) of
            o@(Just h, ((xl, xh) : xs)) ->
               if highestValueInUpperBound h == (pred . lowestValueInLowerBound $ xl)
                  then (Just xh, xs)
                  else o
            x -> x

updateBound :: Bound a -> a -> Bound a
updateBound (Bound _ aType) b = Bound b aType

unmergeRM :: RangeMerge a -> [RangeMerge a]
unmergeRM IRM = [IRM]
unmergeRM (RM lower upper spans) =
   (maybe [] (\x -> [RM Nothing (Just x) []]) upper) ++
   fmap (\x -> RM Nothing Nothing [x]) spans ++
   (maybe [] (\x -> [RM (Just x) Nothing []]) lower)
