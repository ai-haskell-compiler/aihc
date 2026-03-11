module Data.Array.Comfort.Shape.Set where

import Data.Array.Comfort.Shape.Utility (errorIndexFromOffset)

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Tuple.HT (fst3)
import Data.Maybe.HT (toMaybe)


offset :: Ord a => Set a -> a -> Maybe Int
offset set ix =
   case Set.splitMember ix set of
      (less, hit, _) -> toMaybe hit (Set.size less)

uncheckedOffset :: Ord a => Set a -> a -> Int
uncheckedOffset set = Set.size . fst3 . flip Set.splitMember set


indexFromOffset :: Set a -> Int -> Maybe a
indexFromOffset set k = toMaybe (0<=k) (uncheckedIndexFromOffset set k)

uncheckedIndexFromOffset :: Set a -> Int -> a
uncheckedIndexFromOffset set k =
   case drop k $ Set.toAscList set of
      [] -> errorIndexFromOffset "Set" k
      ix:_ -> ix
