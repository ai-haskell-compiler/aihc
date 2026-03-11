-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | A program to solve Sudoku puzzles. It illustrates how to use the
-- MiniSat solver.

module Main where

import SAT.MiniSat

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Char
import System.IO
import System.Environment

-- ----------------------------------------------------------------------
-- * The rules of Sudoku as a SAT problem

-- | 'Cell' /i/ /j/ /n/ is a boolean variable expressing that the cell
-- at row /i/ and column /j/ contains the number /n/.
data Cell = Cell Int Int Int
            deriving (Eq, Ord, Show)

-- | Like the 'Cell' constructor, but return a formula instead of a
-- variable. For convenience.
cell :: Int -> Int -> Int -> Formula Cell
cell i j n = Var (Cell i j n)

-- | The general-purpose Sudoku rules. They state that each cell 
-- contain exactly one number, and each number occurs exactly once in
-- each row, column, and 3x3 square.
rules :: Formula Cell
rules = cells :&&: rows :&&: columns :&&: squares
  where
    cells = All [ ExactlyOne [ cell i j n | n <- [1..9] ] | i <- [1..9], j <- [1..9] ]
    rows = All [ ExactlyOne [ cell i j n | j <- [1..9] ] | i <- [1..9], n <- [1..9] ]
    columns = All [ ExactlyOne [ cell i j n | i <- [1..9] ] | j <- [1..9], n <- [1..9] ]
    squares = All [ ExactlyOne [ cell i j n | i <- [i'..i'+2], j <- [j'..j'+2] ] | i' <- [1,4,7], j' <- [1,4,7], n <- [1..9] ]

-- ----------------------------------------------------------------------
-- * Sudoku solver

-- | A datatype for a partially filled Sudoku.
type Sudoku = Map (Int, Int) Int

-- | Turn a 'Sudoku' into a formula.
formula_of_sudoku :: Sudoku -> Formula Cell
formula_of_sudoku s = All [ cell i j n | ((i,j),n) <- Map.toList s ]

-- | Turn a solution into a 'Sudoku'.
sudoku_of_solution :: Map Cell Bool -> Sudoku
sudoku_of_solution m = Map.fromList [ ((i,j),n) | (Cell i j n, True) <- Map.toList m ] 

-- | Solve the Sudoku. Return all solutions.
solve_sudoku :: Sudoku -> [Sudoku]
solve_sudoku s = map sudoku_of_solution (solve_all (rules :&&: formula_of_sudoku s))

-- ----------------------------------------------------------------------
-- * Pretty-printing

-- | Print a 'Sudoku'.
show_sudoku :: Sudoku -> String
show_sudoku s =
  divider
  ++ concat [ row i | i <- [1,2,3] ] 
  ++ divider
  ++ concat [ row i | i <- [4,5,6] ]
  ++ divider
  ++ concat [ row i | i <- [7,8,9] ]
  ++ divider
  where
    divider = "+-------+-------+-------+\n"
    row i =
      "| "
      ++ intercalate " " [ entry i j | j <- [1, 2, 3]]
      ++ " | "
      ++ intercalate " " [ entry i j | j <- [4, 5, 6]]
      ++ " | "
      ++ intercalate " " [ entry i j | j <- [7, 8, 9]]
      ++ " |\n"
    entry i j = case Map.lookup (i,j) s of
      Nothing -> " "
      Just n -> show n

-- ----------------------------------------------------------------------
-- * Parsing

-- | Create a Sudoku from a list of 81 numbers (using 0 as blank).
sudoku_of_list :: [Int] -> Sudoku
sudoku_of_list xs =
  Map.fromList [ ((i,j),n) | ((i,j),n) <- zip coords xs, 1 <= n && n <= 9 ]
  where
    coords = [ (i,j) | i <- [1..9], j <- [1..9] ]

-- | Read a Sudoku from a string. This accepts the format produced by
-- 'show_sudoku', or a simple list of 81 numbers with 0 representing a
-- blank.
read_sudoku :: String -> Sudoku
read_sudoku s = sudoku_of_list (aux s)
  where
    aux [] = []
    aux (' ':' ':cs) = 0 : aux cs
    aux (c:cs)
      | '0' <= c && c <= '9' = (ord c - ord '0') : aux cs
      | otherwise = aux cs

-- ----------------------------------------------------------------------
-- * Main

-- | Print usage information.
usage :: IO ()
usage = do
  putStrLn "Usage: Sudoku [option]"
  putStrLn "Options:"
  putStrLn " -h, --help  - print usage info and exit"
  putStrLn " -p          - solve a predefined Sudoku (default)"
  putStrLn " -r          - read a Sudoku from stdin and solve"
  putStrLn ""
  putStrLn "The input can be specified in one of two formats:"
  putStrLn "Format 1:"
  putStrLn "+-------+-------+-------+"
  putStrLn "|       |   1 4 |   9   |"
  putStrLn "|   4 7 |     2 |     8 |"
  putStrLn "|   6   |     9 | 2     |"
  putStrLn "+-------+-------+-------+"
  putStrLn "|       |       | 7 6 9 |"
  putStrLn "| 7     |       |     3 |"
  putStrLn "| 5 8 6 |       |       |"
  putStrLn "+-------+-------+-------+"
  putStrLn "|     8 | 2     |   3   |"
  putStrLn "| 6     | 5     | 9 7   |"
  putStrLn "|   7   | 1 4   |       |"
  putStrLn "+-------+-------+-------+"
  putStrLn ""
  putStrLn "Format 2:"
  putStrLn "0 0 0 0 1 4 0 9 0"
  putStrLn "0 4 7 0 0 2 0 0 8"
  putStrLn "0 6 0 0 0 9 2 0 0"
  putStrLn "0 0 0 0 0 0 7 6 9"
  putStrLn "7 0 0 0 0 0 0 0 3"
  putStrLn "5 8 6 0 0 0 0 0 0"
  putStrLn "0 0 8 2 0 0 0 3 0"
  putStrLn "6 0 0 5 0 0 9 7 0"
  putStrLn "0 7 0 1 4 0 0 0 0"


-- | Make the sure the entire string is strictly read before
-- continuing the IO action.
strictly_read :: String -> IO ()
strictly_read [] = return ()
strictly_read (h:t) = strictly_read t

-- | A predefined Sudoku, so the user doesn't have to enter one.
predefined = sudoku_of_list
  [0,0,0,0,1,4,0,9,0,
   0,4,7,0,0,2,0,0,8,
   0,6,0,0,0,9,2,0,0,
   0,0,0,0,0,0,7,6,9,
   7,0,0,0,0,0,0,0,3,
   5,8,6,0,0,0,0,0,0,
   0,0,8,2,0,0,0,3,0,
   6,0,0,5,0,0,9,7,0,
   0,7,0,1,4,0,0,0,0]

-- | The main function.
main :: IO ()
main = do
  args <- getArgs
  case args of
   "--help" : _ -> usage
   "-h" : _ -> usage
   "-r" : _ -> do
     str <- getContents
     strictly_read str
     let s = read_sudoku str
     main_with s
   "-p" : _ -> main_with predefined
   [] -> main_with predefined
   o@('-':_) : _ -> do
     hPutStrLn stderr ("Unrecognized option -- " ++ o)
     hPutStrLn stderr "Try --help for more info."
   _ -> do
     hPutStrLn stderr "Invalid command line. Try --help for more info."

-- | Main-like function for solving the given Sudoku.
main_with :: Sudoku -> IO ()
main_with s = do
  putStrLn "Sudoku:"
  putStr (show_sudoku s)
  case solve_sudoku s of
   [] -> do
     putStrLn "No solution."
   [h] -> do
     putStrLn "Unique solution:"
     putStr (show_sudoku h)
   h:t -> do
     putStrLn "Non-unique solution:"
     putStr (show_sudoku h)
  return ()
