{-# LANGUAGE Safe #-}

-- | This module provides a simple api to access range functionality. It provides standard
-- set operations on ranges, the ability to merge ranges together and, importantly, the ability
-- to check if a value is within a range. The primary benifit of the Range library is performance
-- and versatility.
--
-- __Note:__ It is intended that you will read the documentation in this module from top to bottom.
--
-- = Understanding custom range syntax
--
-- This library supports five different types of ranges:
--
--  * 'SpanRange': A range starting from a value and ending with another value.
--  * 'SingletonRange': This range is really just a shorthand for a range that starts and ends with the same value.
--  * 'LowerBoundRange': A range that starts at a value and extends infinitely in the positive direction.
--  * 'UpperBoundRange': A range that starts at a value and extends infinitely in the negative direction.
--  * 'InfiniteRange': A range that includes all values in your range.
--
-- All of these ranges are bounded in an 'Inclusive' or 'Exclusive' manner.
--
-- To run through a simple example of what this looks like, let's start with mathematical notation and then
-- move into our own notation.
--
-- The bound @[1, 5)@ says "All of the numbers from one to five, including one but excluding 5."
--
-- Using the data types directly, you could write this as:
--
-- @SpanRange (Bound 1 Inclusive) (Bound 5 Exclusive)@
--
-- This is overly verbose, as a result, this library contains operators and functions for writing this much
-- more succinctly. The above example could be written as:
--
-- @1 +=* 5@
--
-- There the @+@ symbol is used to represent the inclusive side of a range and the @*@ symbol is used to represent
-- the exclusive side of a range.
--
-- The 'Show' instance of the 'Range' class will actually output these simplified helper functions, for example:
--
-- >>> [SingletonRange 5, SpanRange (Bound 1 Inclusive) (Bound 5 Exclusive), InfiniteRange]
-- [SingletonRange 5,1 +=* 5,inf]
--
-- There are 'lbi', 'lbe', 'ubi' and 'ube' functions to create lower bound inclusive, lower bound exclusive, upper
-- bound inclusive and upper bound exclusive ranges respectively.
--
-- @SingletonRange x@ is equivalent to @x +=+ x@ but is nicer for presentational purposes in a 'Show'.
--
-- Now that you know the basic syntax to declare ranges, the following uses cases will be easier to understand.
--
-- = Use case 1: Basic Integer Range
--
-- The standard use case for this library is efficiently discovering if an integer is within a given range.
--
-- For example, if we had the range made up of the inclusive unions of @[5, 10]@ and @[20, 30]@ and @[25, Infinity)@
-- then we could instantiate, and simplify, such a range like this:
--
-- >>> mergeRanges [(5 :: Integer) +=+ 10, 20 +=+ 30, lbi 25]
-- [5 +=+ 10,lbi 20]
--
-- You can then test if elements are within this range:
--
-- >>> let ranges = mergeRanges [(5 :: Integer) +=+ 10, 20 +=+ 30, lbi 25]
-- >>> inRanges ranges 7
-- True
-- >>> inRanges ranges 50
-- True
-- >>> inRanges ranges 15
-- False
--
-- The other convenience methods in this library will help you perform more range operations.
--
-- = Use case 2: Version ranges
--
-- All the 'Data.Range' library really needs to work, is the Ord type. If you have a data type that can
-- be ordered, than we can perform range calculations on it. The Data.Version type is an excellent example
-- of this. For example, let's say that you want to say: "I accept a version range of [1.1.0, 1.2.1] or [1.3, 1.4) or [1.4, 1.4.2)"
-- then you can write that as:
--
-- >>> :m + Data.Version
-- >>> let v x = Version x []
-- >>> let ranges = mergeRanges [v [1, 1, 0] +=+ v [1,2,1], v [1,3] +=* v [1,4], v [1,4] +=* v [1,4,2]]
-- >>> inRanges ranges (v [1,0])
-- False
-- >>> inRanges ranges (v [1,5])
-- False
-- >>> inRanges ranges (v [1,1,5])
-- True
-- >>> inRanges ranges (v [1,3,5])
-- True
--
-- As you can see, it is almost identical to the previous example, yet you are now comparing if a version is within a version range!
-- Not only that, but so long as your type is orderable, the ranges can be merged together cleanly.
--
-- With any luck, you can apply this library to your use case of choice. Good luck!
module Data.Range (
      -- * Range creation
      (+=+),
      (+=*),
      (*=+),
      (*=*),
      lbi,
      lbe,
      ubi,
      ube,
      inf,
      -- * Comparison functions
      inRange,
      inRanges,
      aboveRange,
      aboveRanges,
      belowRange,
      belowRanges,
      rangesOverlap,
      rangesAdjoin,
      -- * Set operations
      mergeRanges,
      union,
      intersection,
      difference,
      invert,
      -- * Enumerable methods
      fromRanges,
      joinRanges,
      -- * Data types
      Bound(..),
      BoundType(..),
      Range(..)
   ) where

import Data.Range.Data
import Data.Range.Operators
import Data.Range.Util
import Data.Range.RangeInternal (exportRangeMerge, joinRM, loadRanges)
import qualified Data.Range.Algebra as Alg

-- | Performs a set union between the two input ranges and returns the resultant set of
-- ranges.
--
-- For example:
--
-- >>> union [1 +=+ 10] [5 +=+ (15 :: Integer)]
-- [1 +=+ 15]
-- (0.00 secs, 587,152 bytes)
union :: (Ord a) => [Range a] -> [Range a] -> [Range a]
union a b = Alg.eval $ Alg.union (Alg.const a) (Alg.const b)
{-# INLINE union #-}

-- | Performs a set intersection between the two input ranges and returns the resultant set of
-- ranges.
--
-- For example:
--
-- >>> intersection [1 +=* 10] [5 +=+ (15 :: Integer)]
-- [5 +=* 10]
-- (0.00 secs, 584,616 bytes)
intersection :: (Ord a) => [Range a] -> [Range a] -> [Range a]
intersection a b = Alg.eval $ Alg.intersection (Alg.const a) (Alg.const b)
{-# INLINE intersection #-}

-- | Performs a set difference between the two input ranges and returns the resultant set of
-- ranges.
--
-- For example:
--
-- >>> difference [1 +=+ 10] [5 +=+ (15 :: Integer)]
-- [1 +=* 5]
-- (0.00 secs, 590,424 bytes)
difference :: (Ord a) => [Range a] -> [Range a] -> [Range a]
difference a b = Alg.eval $ Alg.difference (Alg.const a) (Alg.const b)
{-# INLINE difference #-}

-- | An inversion function, given a set of ranges it returns the inverse set of ranges.
--
-- For example:
--
-- >>> invert [1 +=* 10, 15 *=+ (20 :: Integer)]
-- [ube 1,10 +=+ 15,lbe 20]
-- (0.00 secs, 623,456 bytes)
invert :: (Ord a) => [Range a] -> [Range a]
invert = Alg.eval . Alg.invert . Alg.const
{-# INLINE invert #-}

-- | A check to see if two ranges overlap. The ranges overlap if at least one value exists within both ranges.
--  If they do overlap then true is returned; false otherwise.
--
-- For example:
--
-- >>> rangesOverlap (1 +=+ 5) (3 +=+ 7)
-- True
-- >>> rangesOverlap (1 +=+ 5) (5 +=+ 7)
-- True
-- >>> rangesOverlap (1 +=* 5) (5 +=+ 7)
-- False
--
-- The last case of these three is the primary "gotcha" of this method. With @[1, 5)@ and @[5, 7]@ there is no
-- value that exists within both ranges. Therefore, technically, the ranges do not overlap. If you expected
-- this to return True then it is likely that you would prefer to use 'rangesAdjoin' instead.
rangesOverlap :: (Ord a) => Range a -> Range a -> Bool
rangesOverlap a b = Overlap == (rangesOverlapType a b)

rangesOverlapType :: (Ord a) => Range a -> Range a -> OverlapType
rangesOverlapType (SingletonRange a) x = rangesOverlapType (SpanRange b b) x
   where
      b = Bound a Inclusive
rangesOverlapType (SpanRange x y) (SpanRange a b) = boundsOverlapType (x, y) (a, b)
rangesOverlapType (SpanRange _ y) (LowerBoundRange lower) = againstLowerBound y lower
rangesOverlapType (SpanRange x _) (UpperBoundRange upper) = againstUpperBound x upper
rangesOverlapType (LowerBoundRange _) (LowerBoundRange _) = Overlap
rangesOverlapType (LowerBoundRange lower) (UpperBoundRange upper) = againstUpperBound lower upper
rangesOverlapType (UpperBoundRange _) (UpperBoundRange _) = Overlap
rangesOverlapType InfiniteRange _ = Overlap
rangesOverlapType a b = rangesOverlapType b a

-- | A check to see if two ranges overlap or adjoin. The ranges adjoin if no values exist between them.
--  If they do overlap or adjoin then true is returned; false otherwise.
--
-- For example:
--
-- >>> rangesAdjoin (1 +=+ 5) (3 +=+ 7)
-- True
-- >>> rangesAdjoin (1 +=+ 5) (5 +=+ 7)
-- True
-- >>> rangesAdjoin (1 +=* 5) (5 +=+ 7)
-- True
--
-- The last case of these three is the primary "gotcha" of this method. With @[1, 5)@ and @[5, 7]@ there
-- exist no values between them. Therefore the ranges adjoin. If you expected this to return False then
-- it is likely that you would prefer to use 'rangesOverlap' instead.
rangesAdjoin :: (Ord a) => Range a -> Range a -> Bool
rangesAdjoin a b = Adjoin == (rangesOverlapType a b)

-- | Given a range and a value it will tell you wether or not the value is in the range.
-- Remember that all ranges are inclusive.
--
-- The primary value of this library is performance and this method can be used to show
-- this quite clearly. For example, you can try and approximate basic range functionality
-- with "Data.List.elem" so we can generate an apples to apples comparison in GHCi:
--
-- >>> :set +s
-- >>> elem (10000000 :: Integer) [1..10000000]
-- True
-- (0.26 secs, 720,556,888 bytes)
-- >>> inRange (1 +=+ 10000000) (10000000 :: Integer)
-- True
-- (0.00 secs, 557,656 bytes)
-- >>>
--
-- As you can see, this function is significantly more performant, in both speed and memory,
-- than using the elem function.
inRange :: (Ord a) => Range a -> a -> Bool
inRange (SingletonRange a) value = value == a
inRange (SpanRange x y) value = Overlap == boundIsBetween (Bound value Inclusive) (x, y)
inRange (LowerBoundRange lower) value = Overlap == againstLowerBound (Bound value Inclusive) lower
inRange (UpperBoundRange upper) value = Overlap == againstUpperBound (Bound value Inclusive) upper
inRange InfiniteRange _ = True

-- | Given a list of ranges this function tells you if a value is in any of those ranges.
-- This is especially useful for more complex ranges.
inRanges :: (Ord a) => [Range a] -> a -> Bool
inRanges rs a = any (`inRange` a) rs

-- | Checks if the value provided is above (or greater than) the biggest value in
-- the given range.
--
-- The "LowerBoundRange" and the "InfiniteRange" will always
-- cause this method to return False because you can't have a value
-- higher than them since they are both infinite in the positive
-- direction.
--
-- >>> aboveRange (SingletonRange 5) (6 :: Integer)
-- True
-- >>> aboveRange (1 +=+ 5) (6 :: Integer)
-- True
-- >>> aboveRange (1 +=+ 5) (0 :: Integer)
-- False
-- >>> aboveRange (lbi 0) (6 :: Integer)
-- False
-- >>> aboveRange (ubi 0) (6 :: Integer)
-- True
-- >>> aboveRange inf (6 :: Integer)
-- False
aboveRange :: (Ord a) => Range a -> a -> Bool
aboveRange (SingletonRange a)       value = value > a
aboveRange (SpanRange _ y)          value = Overlap == againstLowerBound (Bound value Inclusive) (invertBound y)
aboveRange (LowerBoundRange _)      _     = False
aboveRange (UpperBoundRange upper)  value = Overlap == againstLowerBound (Bound value Inclusive) (invertBound upper)
aboveRange InfiniteRange            _     = False

-- | Checks if the value provided is above all of the ranges provided.
aboveRanges :: (Ord a) => [Range a] -> a -> Bool
aboveRanges rs a = all (`aboveRange` a) rs

-- | Checks if the value provided is below (or less than) the smallest value in
-- the given range.
--
-- The "UpperBoundRange" and the "InfiniteRange" will always
-- cause this method to return False because you can't have a value
-- lower than them since they are both infinite in the negative
-- direction.
--
-- >>> belowRange (SingletonRange 5) (4 :: Integer)
-- True
-- >>> belowRange (1 +=+ 5) (0 :: Integer)
-- True
-- >>> belowRange (1 +=+ 5) (6 :: Integer)
-- False
-- >>> belowRange (lbi 6) (0 :: Integer)
-- True
-- >>> belowRange (ubi 6) (0 :: Integer)
-- False
-- >>> belowRange inf (6 :: Integer)
-- False
belowRange :: (Ord a) => Range a -> a -> Bool
belowRange (SingletonRange a)       value = value < a
belowRange (SpanRange x _)          value = Overlap == againstUpperBound (Bound value Inclusive) (invertBound x)
belowRange (LowerBoundRange lower)  value = Overlap == againstUpperBound (Bound value Inclusive) (invertBound lower)
belowRange (UpperBoundRange _)      _     = False
belowRange InfiniteRange            _     = False

-- | Checks if the value provided is below all of the ranges provided.
belowRanges :: (Ord a) => [Range a] -> a -> Bool
belowRanges rs a = all (`belowRange` a) rs

-- | An array of ranges may have overlaps; this function will collapse that array into as few
-- Ranges as possible. For example:
--
-- >>> mergeRanges [lbi 12, 1 +=+ 10, 5 +=+ (15 :: Integer)]
-- [lbi 1]
-- (0.01 secs, 588,968 bytes)
--
-- As you can see, the mergeRanges method collapsed multiple ranges into a single range that
-- still covers the same surface area.
--
-- This may be useful for a few use cases:
--
--  * You are hyper concerned about performance and want to have the minimum number of ranges
--    for comparison in the inRanges function.
--  * You wish to display ranges to a human and want to show the minimum number of ranges to
--    avoid having to make people perform those calculations themselves.
--
-- Please note that the use of any of the operations on sets of ranges like invert, union and
-- intersection will have the same behaviour as mergeRanges as a side effect. So, for example,
-- this is redundant:
--
-- @
-- mergeRanges . union []
-- @
mergeRanges :: (Ord a) => [Range a] -> [Range a]
mergeRanges = Alg.eval . Alg.union (Alg.const []) . Alg.const
{-# INLINE mergeRanges #-}

-- | Instantiate all of the values in a range.
--
-- __Warning__: This method is meant as a convenience method, it is not efficient.
--
-- A set of ranges represents a collection of real values without actually instantiating
-- those values. Not instantiating ranges, allows the range library to support infinite
-- ranges and be super performant.
--
-- However, sometimes you actually want to get the values that your range represents, or even
-- get a sample set of the values. This function generates as many of the values that belong
-- to your range as you like.
--
-- Because ranges can be infinite, it is highly recommended to combine this method with something like
-- "Data.List.take" to avoid an infinite recursion.
--
-- This method will attempt to take a sample from all of the ranges that you have provided, however
-- it is not guaranteed that you will get an even sampling. All that is guaranteed is that you will
-- only get back values that are within one or more of the ranges you provide.
--
-- == Examples
--
-- A simple span:
--
-- >>> take 5 . fromRanges $ [1 +=+ 10 :: Range Integer, 20 +=+ 30]
-- [1,20,2,21,3]
-- (0.01 secs, 566,016 bytes)
--
-- An infinite range:
--
-- >>> take 5 . fromRanges $ [inf :: Range Integer]
-- [0,1,-1,2,-2]
-- (0.00 secs, 566,752 bytes)
fromRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromRanges = takeEvenly . fmap fromRange . mergeRanges
   where
      fromRange range = case range of
         SingletonRange x -> [x]
         SpanRange (Bound a aType) (Bound b bType) -> [(if aType == Inclusive then a else succ a)..(if bType == Inclusive then b else pred b)]
         LowerBoundRange (Bound x xType) -> iterate succ (if xType == Inclusive then x else succ x)
         UpperBoundRange (Bound x xType) -> iterate pred (if xType == Inclusive then x else pred x)
         InfiniteRange -> zero : takeEvenly [tail $ iterate succ zero, tail $ iterate pred zero]
            where
               zero = toEnum 0

-- | Joins together ranges that we only know can be joined because of the 'Enum' class.
--
-- To make the purpose of this method easier to understand, let's run throuh a simple example:
--
-- >>> mergeRanges [1 +=+ 5, 6 +=+ 10] :: [Range Integer]
-- [1 +=+ 5,6 +=+ 10]
--
-- In this example, you know that the values are all of the type 'Integer'. Because of this, you
-- know that there are no values between 5 and 6. You may expect that the `mergeRanges` function
-- should "just know" that it can merge these together; but it can't because it does not have the
-- required constraints. This becomes more obvious if you modify the example to use 'Double' instead:
--
-- >>> mergeRanges [1.5 +=+ 5.5, 6.5 +=+ 10.5] :: [Range Double]
-- [1.5 +=+ 5.5,6.5 +=+ 10.5]
--
-- Now we can see that there are an infinite number of values between 5.5 and 6.5 and thus no such 
-- join between the two ranges could occur.
--
-- This function, joinRanges, provides the missing piece that you would expect:
--
-- >>> joinRanges $ mergeRanges [1 +=+ 5, 6 +=+ 10] :: [Range Integer]
-- [1 +=+ 10]
--
-- You can use this method to ensure that all ranges for whom the value implements 'Enum' can be
-- compressed to their smallest representation.
joinRanges :: (Ord a, Enum a) => [Range a] -> [Range a]
joinRanges = exportRangeMerge . joinRM . loadRanges
