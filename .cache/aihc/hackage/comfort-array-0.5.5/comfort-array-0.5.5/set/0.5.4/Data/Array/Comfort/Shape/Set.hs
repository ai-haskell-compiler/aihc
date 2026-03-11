module Data.Array.Comfort.Shape.Set where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe.HT (toMaybe)


offset :: Ord a => Set a -> a -> Maybe Int
offset = flip Set.lookupIndex

uncheckedOffset :: Ord a => Set a -> a -> Int
uncheckedOffset = flip Set.findIndex


indexFromOffset :: Set a -> Int -> Maybe a
indexFromOffset set k =
   toMaybe (0<=k && k<Set.size set) (Set.elemAt k set)

uncheckedIndexFromOffset :: Set a -> Int -> a
uncheckedIndexFromOffset = flip Set.elemAt
