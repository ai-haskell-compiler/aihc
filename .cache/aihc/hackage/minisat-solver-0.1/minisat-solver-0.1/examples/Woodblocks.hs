-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | A program to solve the Woodblocks puzzle. It illustrates how to use
-- the MiniSat solver.
--
-- The objective of the Woodblocks puzzle is to fit 27 blocks of
-- dimensions /A/×/B/×/C/ into a cube of side length /A/+/B/+/C/.
-- This puzzle was described by D. G. Hoffman in /David Klarner,/
-- /editor, \"The Mathematical Gardner\", pp. 211-225, Springer, 1981/.
--
-- We assume that 0 < (/A/+/B/+/C/)\/4 < /A/ < /B/ < /C/ <
-- (/A/+/B/+/C/)\/2. For concreteness, we use /A/=4, /B/=5, /C/=6.
--
-- We solve the puzzle by reducing it to a SAT problem, then using the
-- "SAT.MiniSat" library to find all 21 solutions.
--
-- For fun, we provide a 'main' function that can either output the
-- solutions in ASCII form, or graphically as a PDF or PostScript
-- document.

module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.EasyRender hiding (X, Y)
import System.Environment
import System.IO

import SAT.MiniSat

-- ----------------------------------------------------------------------
-- * Encoding the puzzle as a SAT problem

-- | An axis is 'X', 'Y', or 'Z'. We use the letter /s/ to denote a
-- generic axis.
data Axis = X | Y | Z
          deriving (Show, Eq, Ord)

-- | A length is 'A', 'B', or 'C'. These symbolic constants represent
-- the values /A/=4, /B/=5, and /C/=6, respectively.
--
-- A length in the 'X'-direction is also called \"width\" or 'X'-length.
-- A length in the 'Y'-direction is also called \"depth\" or 'Y'-length.
-- A length in the 'Z'-direction is also called \"height\" or 'Z'-length.
data Length = A | B | C
         deriving (Show, Eq, Ord)

-- | The members of the 'Size' type are symbolic boolean variables.
-- Specifically, the variable
--
-- > Size i j k s n
--
-- is 'True' if and only if the block at position (/i/, /j/, /k/) has
-- /s/-length /n/. This illustrates the use of a structured type as
-- the type of boolean variables in the SAT solver.
data Size = Size Int Int Int Axis Length
          deriving (Show, Eq, Ord)

-- | Like the 'Size' constructor, but return a formula instead of a
-- variable. For convenience.
size :: Int -> Int -> Int -> Axis -> Length -> Formula Size
size i j k s n = Var (Size i j k s n)

-- | Each of the 27 blocks is identified by a triple (/i/, /j/, /k/) of
-- coordinates, ranging from (0, 0, 0) to (2, 2, 2).
type Block = (Int, Int, Int)

-- | The set of all blocks.
blocks :: [(Int, Int, Int)]
blocks = [ (i, j, k) | i <- [0..2], j <- [0..2], k <- [0..2] ]

-- ----------------------------------------------------------------------
-- ** Basic constraints

-- | Functionality constraints: assert that 'Size' encodes a
-- function, i.e., for all /i/, /j/, /k/, /s/, there exists exactly
-- one /n/ such that 'Size' /i/ /j/ /k/ /s/ /n/.
functionality_constraints :: Formula Size
functionality_constraints = All [ ExactlyOne [ size i j k s n | n <- [A, B, C] ] | (i, j, k) <- blocks, s <- [X, Y, Z] ]

-- | Block constraints: assert that each block has exactly
-- one side each of lengths /A/, /B/, /C/.
block_constraints :: Formula Size
block_constraints = All [ ExactlyOne [ size i j k s n | s <- [X, Y, Z] ] | (i, j, k) <- blocks, n <- [A, B, C] ]

-- | Width constraints: assert that each row of blocks
-- contains exactly one block each of lengths /A/, /B/, /C/. This is
-- an easy-to-prove property of the Woodblocks puzzle.
width_constraints :: Formula Size
width_constraints = All [ ExactlyOne [ size i j k X n | i <- [0..2] ] | j <- [0..2], k <- [0..2], n <- [A, B, C] ]

-- | Depth constraints: like 'width_constraints', but in the
-- 'Y'-direction.
depth_constraints :: Formula Size
depth_constraints = All [ ExactlyOne [ size i j k Y n | j <- [0..2] ] | i <- [0..2], k <- [0..2], n <- [A, B, C] ]

-- | Height constraints: like 'width_constraints', but in the
-- 'Z'-direction.
height_constraints :: Formula Size
height_constraints = All [ ExactlyOne [ size i j k Z n | k <- [0..2] ] | i <- [0..2], j <- [0..2], n <- [A, B, C] ]

-- ----------------------------------------------------------------------
-- ** Overlap constraints

-- $ The basic constraints are sufficient to ensure that adjacent
-- blocks (i.e., those that have a face in common) do not overlap.
-- However, we must also ensure that blocks don't overlap along an
-- edge or a corner. The overlap constraints ensure this. We
-- symbolically compute the minimal and maximal /x/-, /y/-, and
-- /z/-coordinate of each block. We use the fact that two blocks are
-- non-overlapping iff
--
-- x1' ≤ x0 or x1 ≤ x0' or y1' ≤ y0 or y1 ≤ y0' or z1' ≤ z0 or z1 ≤ z0'.

-- | 'Coord' is a symbolic representation of a coordinate, i.e., a
-- number. These are relative to 0, or relative to the quantity /L/ =
-- /A/+/B/+/C/.
data Coord =
  Zero                        -- ^ The number 0.
  | LengthOf Axis Block       -- ^ The /s/-length of block /b/.
  | LMinusLengthOf Axis Block -- ^ /L/ minus the /s/-length of block /b/.
  | L                         -- ^ The number /L/.
    deriving (Show)

-- | Symbolically calculate the minimum /s/-coordinate of block
-- (/i/, /j/, /k/).
mincoord :: Axis -> Block -> Coord
mincoord X (0, j, k) = Zero
mincoord X (1, j, k) = LengthOf X (0, j, k)
mincoord X (2, j, k) = LMinusLengthOf X (2, j, k)
mincoord X (3, j, k) = L
mincoord Y (i, 0, k) = Zero
mincoord Y (i, 1, k) = LengthOf Y (i, 0, k)
mincoord Y (i, 2, k) = LMinusLengthOf Y (i, 2, k)
mincoord Y (i, 3, k) = L
mincoord Z (i, j, 0) = Zero
mincoord Z (i, j, 1) = LengthOf Z (i, j, 0)
mincoord Z (i, j, 2) = LMinusLengthOf Z (i, j, 2)
mincoord Z (i, j, 3) = L
mincoord _ (_, _, _) = error "mincoord"

-- | Symbolically calculate the maximum /s/-coordinate of block
-- (/i/, /j/, /k/).
maxcoord :: Axis -> Block -> Coord
maxcoord X (i, j, k) = mincoord X (i+1, j, k)
maxcoord Y (i, j, k) = mincoord Y (i, j+1, k)
maxcoord Z (i, j, k) = mincoord Z (i, j, k+1)

-- | 'leq_length' /b1/ /s1/ /b2/ /s2/: a constraint asserting that the
-- /s1/-length of block /b1/ is less than or equal to to /s2/-length
-- of block /b2/.
leq_length :: Block -> Axis -> Block -> Axis -> Formula Size
leq_length (i, j, k) s (i', j', k') s' =
  size i j k s A :||: size i' j' k' s' C :||: (size i j k s B :&&: size i' j' k' s' B)

-- | 'leq' /c1/ /c2/: a constraint asserting that one formal coordinate is
-- less than or equal another.
leq :: Coord -> Coord -> Formula Size
leq Zero _ = Yes
leq _ Zero = No
leq (LengthOf s1 b1) (LengthOf s2 b2) = leq_length b1 s1 b2 s2
leq (LengthOf s1 b1) _ = Yes
leq _ (LengthOf s2 b2) = No
leq (LMinusLengthOf s1 b1) (LMinusLengthOf s2 b2) = leq_length b2 s2 b1 s1
leq (LMinusLengthOf s1 b1) _ = Yes
leq L (LMinusLengthOf s2 b2) = No
leq L L = Yes

-- | 'disjoint' /b1/ /b2/ is a constraint asserting that blocks /b1/
-- and /b2/ do not overlap.
disjoint :: Block -> Block -> Formula Size
disjoint b1 b2 =
  leq (maxcoord X b1) (mincoord X b2)
  :||: leq (maxcoord Y b1) (mincoord Y b2)
  :||: leq (maxcoord Z b1) (mincoord Z b2)
  :||: leq (maxcoord X b2) (mincoord X b1)
  :||: leq (maxcoord Y b2) (mincoord Y b1)
  :||: leq (maxcoord Z b2) (mincoord Z b1)

-- | Overlap constraints: assert that no two distinct blocks overlap.
overlap_constraints :: Formula Size
overlap_constraints = All [ disjoint b1 b2 | b1 <- blocks, b2 <- blocks, b1 /= b2 ]

-- ----------------------------------------------------------------------
-- ** Symmetry constraints

-- | Without loss of generality (up to symmetry), we can fix the
-- orientation of the central block, as well as the /s/-lengths of
-- its /s/-neighbors, for /s/ ∈ {/X/, /Y/, /Z/}.
symmetry_constraints :: Formula Size
symmetry_constraints =
  size 1 1 1 X A
  :&&: size 1 1 1 Y B
  :&&: size 1 1 1 Z C
  :&&: size 0 1 1 X B
  :&&: size 2 1 1 X C
  :&&: size 1 0 1 Y A
  :&&: size 1 2 1 Y C
  :&&: size 1 1 0 Z A
  :&&: size 1 1 2 Z B

-- ----------------------------------------------------------------------
-- ** Summary of constraints

-- | The complete set of puzzle constraints.
puzzle :: Formula Size
puzzle =
  functionality_constraints
  :&&: block_constraints
  :&&: width_constraints
  :&&: depth_constraints
  :&&: height_constraints
  :&&: overlap_constraints
  :&&: symmetry_constraints

-- ----------------------------------------------------------------------
-- * Solving the puzzle

-- | A solution can be represented as a function mapping each triple of
-- block coordinates (/i/, /j/, /k/) to an orientation.
type Solution = Int -> Int -> Int -> (Length, Length, Length)

-- | Map a solution of the SAT formula to a solution of the puzzle.
solution_of_sat :: Map Size Bool -> Solution
solution_of_sat m = f
  where
    m' = Map.fromList [ ((i, j, k, s), n) | (Size i j k s n, True) <- Map.toList m ]
    f i j k = (x, y, z)
      where
        x = m' Map.! (i, j, k, X)
        y = m' Map.! (i, j, k, Y)
        z = m' Map.! (i, j, k, Z)

-- | The set of all solutions to the Woodblocks puzzle. 
solutions :: [Solution]
solutions = map solution_of_sat sat_solutions
  where
    sat_solutions = solve_all puzzle

-- ----------------------------------------------------------------------
-- * ASCII output

-- | Show a list, using the given arguments /nil/, /left/, /comma/,
-- /right/, and the given /show/ function for elements.
show_list :: String -> String -> String -> String -> (a -> String) -> [a] -> String
show_list nil left comma right show [] = nil
show_list nil left comma right show xs = left ++ intercalate comma [ show x | x <- xs ] ++ right

-- | Show the block sizes in a grid.
show_solution :: Solution -> String
show_solution f = "Solution:\n" ++ show_list "" "" "\n" "" (show_list "" "" "\n" "\n" (show_list "" "" " " "" show)) xs
  where
    xs = [ [ [ f i j k | i <- [0,1,2] ] | j <- [0,1,2] ] | k <- [0,1,2] ]

-- | Main function for ASCII output.
main_ascii :: IO ()
main_ascii = sequence_ [ print_sol s | s <- solutions ]
  where
    print_sol s = do
      putStrLn (show_solution s)

-- ----------------------------------------------------------------------
-- * Graphical output

-- | A point in 3-space.
type Point = (Int, Int, Int)

-- | A box in 3-space.
type Box = (Point, Point)

-- | Convert a dimension to an integer.
int_of_dim :: Length -> Int
int_of_dim A = 4
int_of_dim B = 5
int_of_dim C = 6

-- | Calculate the minimum /s/-coordinate of block (/i/,/j/,/k/).
smin :: Solution -> Axis -> Int -> Int -> Int -> Int
smin sol X i j k = sum [ int_of_dim x | i' <- [0..i-1], let (x,y,z) = sol i' j k ]
smin sol Y i j k = sum [ int_of_dim y | j' <- [0..j-1], let (x,y,z) = sol i j' k ]
smin sol Z i j k = sum [ int_of_dim z | k' <- [0..k-1], let (x,y,z) = sol i j k' ]

-- | Calculate a block's box.
box_of_block :: Solution -> Int -> Int -> Int -> Box
box_of_block sol i j k = ((x0, y0, z0), (x1, y1, z1))
  where
    x0 = smin sol X i j k
    x1 = smin sol X (i+1) j k
    y0 = smin sol Y i j k
    y1 = smin sol Y i (j+1) k
    z0 = smin sol Z i j k
    z1 = smin sol Z i j (k+1)

-- | Display a box as a rectangle. This is relative to the current
-- coordinate system. The height is printed in the center of the box.
rectangle_of_box :: Box -> Draw ()
rectangle_of_box ((x0, y0, z0), (x1, y1, z1)) = do
  rectangle x0' y0' (x1'-x0') (y1'-y0')
  fillstroke fillcolor
  -- textbox 0.5 font textcolor x0' yc' x1' yc' 0.3 (show (z1 - z0))
  where
    font = Font TimesRoman 2
    textcolor = Color_Gray 0.0
    fillcolor = Color_Gray ((8 + z0' - z1') / 5)
    x0' = fromIntegral x0
    x1' = fromIntegral x1
    y0' = fromIntegral y0
    y1' = fromIntegral y1
    z0' = fromIntegral z0
    z1' = fromIntegral z1
    yc' = (y0' + y1') / 2

-- | Display a layer relative to the current coordinate system.
draw_layer :: Solution -> Int -> Draw ()
draw_layer sol k = do
  -- Todo: add background
  sequence_ [ rectangle_of_box (box_of_block sol i j k) | i <- [0..2], j <- [0..2] ]
  -- Todo: overlay grid

-- | Display a solution, relative to the current coordinate system.
draw_solution :: Solution -> Draw ()
draw_solution sol = do
  block $ do
    draw_layer sol 0
    translate 17.5 0
    draw_layer sol 1
    translate 17.5 0
    draw_layer sol 2

-- | Display a list of solutions, relative to the current coordinate system.
draw_solutions :: [Solution] -> Draw ()
draw_solutions sols = block $ aux sols
  where
    aux [] = return ()
    aux (h:t) = do
      translate 0 (-15)
      draw_solution h
      translate 0 (-5)
      aux t

-- | Divide a list into sublists of length /n/.
paginate :: Int -> [a] -> [[a]]
paginate n [] = []
paginate n xs = x1s : paginate n x2s
  where
    (x1s, x2s) = splitAt n xs

-- | Display a list of solutions as a document.
document_of_solutions :: [Solution] -> Document ()
document_of_solutions sols = do
  sequence_ [ dopage ss | ss <- solss ]
  where
    solss = paginate 6 sols -- solutions per page
    dopage ss = do
      newpage (72*8.5) (72*11) $ do
        -- Set up coordinate system: origin is 1 inch from upper left
        scale 72 72
        translate 4.25 10
        scale (9/115) (9/115)
        translate (-25) 0
        setlinewidth 0.2
        draw_solutions ss

-- | Main function for graphical output.
main_graphical :: RenderFormat -> IO ()
main_graphical fmt = do
  let document = document_of_solutions solutions
  render_stdout fmt document

-- ----------------------------------------------------------------------
-- * Main

-- | Print usage information.
usage :: IO ()
usage = do
  putStrLn "Usage: Woodblocks [option]"
  putStrLn "Options:"
  putStrLn " -h, --help  - print usage info and exit"
  putStrLn " -a          - ASCII output (default)"
  putStrLn " -f          - PDF output"
  putStrLn " -p          - PostScript output"

-- | The main function.
main = do
  args <- getArgs
  case args of
   "--help" : _ -> usage
   "-h" : _ -> usage
   "-f" : _ -> main_graphical Format_PDF
   "-p" : _ -> main_graphical Format_PS
   "-a" : _ -> main_ascii
   [] -> main_ascii
   o@('-':_) : _ -> do
     hPutStrLn stderr ("Unrecognized option -- " ++ o)
     hPutStrLn stderr "Try --help for more info."
   _ -> do
     hPutStrLn stderr "Invalid command line. Try --help for more info."
