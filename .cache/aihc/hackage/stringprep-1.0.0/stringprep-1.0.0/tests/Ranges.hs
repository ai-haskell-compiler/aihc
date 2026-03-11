module Ranges
where

import Data.Set (Set)
import qualified Data.Set as Set

data Ord a => Range a = Single !a | Range !a !a
instance (Ord a, Show a) => Show (Range a) where
        show (Single x) = concat ["(", show x, ")"]
        show (Range x y) = concat ["(", show x, "–", show y, ")"]

newtype Ord a => Ranges a = Ranges [Range a] deriving Show

-- | A rather hacked-up instance.
-- This is to support fast lookups using 'Data.Set' (see 'toSet').
instance (Ord a) => Eq (Range a) where
        (Single x) == (Single y) = x == y
        (Single a) == (Range x y) = x <= a && a <= y
        (Range x y) == (Single a) = x <= a && a <= y
        (Range lx ux) == (Range ly uy) = (lx <= uy && ux >= ly) || (ly <= ux && uy >= lx)

instance (Ord a) => Ord (Range a) where
        (Single x) <= (Single y) = x <= y
        (Single x) <= (Range y _) = x <= y
        (Range _ x) <= (Single y) = x <= y
        (Range _ x) <= (Range y _) = x <= y

-- | A range consisting of a single value.
single :: (Ord a) => a -> Range a
single x = Single x

-- | Construct a 'Range' from a lower and upper bound.
range :: (Ord a) => a -> a -> Range a
range l u
        | l <= u = Range l u
        | otherwise = error "lower bound must be smaller than upper bound"

-- | Construct a 'Ranges' from a list of lower and upper bounds.
ranges :: (Ord a) => [Range a] -> Ranges a
ranges = Ranges . foldr (flip mergeRanges) []

-- | Tests if a given range contains a particular value.
inRange :: (Ord a) => a -> Range a -> Bool
inRange x y = Single x == y

-- | Tests if any of the ranges contains a particular value.
inRanges :: (Ord a) => a -> Ranges a -> Bool
inRanges x (Ranges xs) = or . map (x `inRange`) $ xs

mergeRange :: (Ord a) => Range a -> Range a -> Either (Range a) (Range a)
mergeRange x y =
        if x == y
                then Right $ minMax x y
                else Left $ x

minMax :: (Ord a) => Range a -> Range a -> Range a
minMax (Range lx ux) (Range ly uy) = Range (min lx ly) (max ux uy)
minMax (Single _) y = y
minMax x@(Range _ _) (Single _) = x

-- | Allows quick lookups using ranges.
toSet :: (Ord a) => Ranges a -> Set (Range a)
toSet (Ranges x) = Set.fromList x

addRange :: (Ord a) => Ranges a -> Range a -> Ranges a
addRange (Ranges x) = Ranges . mergeRanges x

mergeRanges :: (Ord a) => [Range a] -> Range a -> [Range a]
mergeRanges [] y = [y]
mergeRanges (x:xs) y = case mergeRange x y of
                Right z -> mergeRanges xs z
                Left x -> x : (mergeRanges xs y)
