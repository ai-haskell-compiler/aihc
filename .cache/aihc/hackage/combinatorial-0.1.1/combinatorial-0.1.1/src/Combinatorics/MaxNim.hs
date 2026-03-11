{- |
Simulation of a game with the following rules:

Players A and B alternatingly take numbers from a set of 2*n numbers.
Player A can choose freely from the remaining numbers,
whereas player B always chooses the maximum remaining number.
How many possibly outcomes of the games exist?
The order in which the numbers are taken is not respected.

E-Mail by Daniel Beer from 2011-10-24.
-}
module Combinatorics.MaxNim (numberOfPossibilities) where

import qualified Data.Set as Set


{- |
We only track the number taken by player A
because player B will automatically have the complement set.
-}
gameRound :: (Set.Set Int, Set.Set Int) -> [(Set.Set Int, Set.Set Int)]
gameRound (takenByA, remaining) = do
   a <- Set.toList remaining
   return (Set.insert a takenByA, Set.deleteMax $ Set.delete a remaining)

possibilities :: Int -> Set.Set (Set.Set Int)
possibilities n =
   Set.fromList $ map fst $
   foldl (>>=) [(Set.empty, Set.fromList [1 .. 2*n])] $
   replicate n gameRound

{-
This turns out to be the sequence of Catalan numbers.
-}
numberOfPossibilities :: [Int]
numberOfPossibilities =
   map (Set.size . possibilities) [0..]
