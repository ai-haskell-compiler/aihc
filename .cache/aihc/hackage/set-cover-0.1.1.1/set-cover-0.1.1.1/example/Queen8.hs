{- |
Place 8 queens on a chessboard
such that no queen threatens another one.

<http://en.wikipedia.org/wiki/Eight_queens_puzzle>

The solutions could be found pretty simply by an exhaustive search.
Nonetheless I like to use this as a simple example
for demonstrating how to use the @set-cover@ library.
-}
module Main where

import qualified Math.SetCover.Exact as ESC

import Control.Monad (liftM2)

import qualified Data.Array as Array
import qualified Data.Set as Set
import Data.Array (accumArray)
import Data.Set (Set)
import Data.List.HT (sliceVertical)
import Data.List (intersperse)
import Data.Maybe (catMaybes)


n :: Int
n = 8

range :: [Int]
range = [0 .. n-1]


data X = Row Int | Column Int | Diag Int | Gaid Int
         deriving (Eq, Ord, Show)

type Assign = ESC.Assign (Maybe (Int, Int)) (Set X)

{- |
'assign' represents a queen at a particular position.

Every queen blocks a row, a column and two diagonals.
Conversely, every row and every column must contain a queen.
This is expressed by the fact that the set partition must contain every element
that is contained in any of the sets we pass to ESC.partitions.
This way we ensure that exactly 8 queens are placed.

Since the search algorithm treats every element the same way,
the generic algorithm chooses in every step
a row, a column or a diagonal
with the smallest number of possibilities to place a queen.
-}
assign :: Int -> Int -> Assign
assign i j =
   ESC.assign (Just (i,j)) $
   Set.fromList [Row i, Column j, Diag (i+j), Gaid (i-j)]

{- |
'fill' represents a diagonal without a queen.

The rationale is this:
Every queen blocks a row and a column
and conversely in each row and in each column there is a queen.
This is not true for diagonals.
There are 15 diagonals in up-right direction, but only 8 queens.
Thus we fill empty diagonals with auxiliary singleton sets,
where each such set addresses one diagonal.
-}
fill :: X -> Assign
fill = ESC.assign Nothing . Set.singleton

assigns :: [Assign]
assigns =
   liftM2 assign range range
   ++
   map (fill . Diag) [0 .. 2*(n-1)]
   ++
   map (fill . Gaid) [1-n .. n-1]

format :: [Maybe (Int,Int)] -> String
format =
   unlines . map (intersperse ' ') . sliceVertical n . Array.elems .
   accumArray (flip const) '.' ((0,0),(n-1,n-1)) .
   map (flip (,) 'Q') . catMaybes


main :: IO ()
main = mapM_ (putStrLn . format) $ ESC.partitions assigns
