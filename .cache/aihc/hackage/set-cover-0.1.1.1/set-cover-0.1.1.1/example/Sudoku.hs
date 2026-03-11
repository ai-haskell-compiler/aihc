{-
<https://en.wikipedia.org/wiki/Sudoku>
-}
module Main where

import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import qualified Math.SetCover.Exact as ESC

import Data.Word (Word32, Word64)

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM3, guard)

import qualified Random as Random
import System.Random (StdGen, getStdGen, )

import qualified Graphics.Ascii.Haha.Terminal as ANSI
import Text.Printf (printf)

import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (foldMap, forM_)
import Data.Array (array, listArray)
import Data.IntSet (IntSet)
import Data.Set (Set)
import Data.List.HT (sliceVertical)
import Data.List (intersperse, intercalate)
import Data.Tuple.HT (mapSnd)


data X = Pos Int Int | Row Int Int | Column Int Int | Square Int Int Int
         deriving (Eq, Ord, Show)

type Cell = ((Int, Int), Int)
type Assign = ESC.Assign Cell

assign :: Int -> Int -> Int -> Assign (Set X)
assign k i j =
   ESC.assign ((i,j), k) $
   Set.fromList [Pos i j, Row k i, Column k j, Square k (div i 3) (div j 3)]

assigns :: [Assign (Set X)]
assigns = liftM3 assign [1..9] [0..8] [0..8]


type Word81 = Bit.Sum Word64 Word32
type Mask =
   BitSet.Set
      (Bit.Sum
         (Bit.Sum Word81 Word81)
         (Bit.Sum Word81 Word81))

bit9x9 :: Int -> Int -> Word81
bit9x9 i j =
   let k = i*9+j
   in  if k<64
         then Bit.bitLeft k
         else Bit.bitRight (k-64)

bitAssign :: Int -> Int -> Int -> Assign Mask
bitAssign k i j =
   ESC.assign ((i,j), k) $
   BitSet.Set $
   Bit.Sum
      (Bit.Sum (bit9x9 k i) (bit9x9 k j))
      (Bit.Sum (bit9x9 i j) (bit9x9 k (div i 3 + 3 * div j 3)))

bitAssigns :: [Assign Mask]
bitAssigns = liftM3 bitAssign [1..9] [0..8] [0..8]


type BitVector = BitSet.Set Integer

bitVectorAssigns :: [Assign BitVector]
bitVectorAssigns = ESC.bitVectorFromSetAssigns assigns

intSetAssigns :: [Assign IntSet]
intSetAssigns = ESC.intSetFromSetAssigns assigns


format :: [Cell] -> String
format =
   unlines . map (intersperse ' ') . sliceVertical 9 . Array.elems .
   fmap (\n -> toEnum $ n + fromEnum '0') .
   array ((0,0),(8,8))

formatSparse :: [Cell] -> String
formatSparse =
   unlines . map (intersperse ' ') . sliceVertical 9 . Array.elems .
   (listArray ((0,0),(8,8)) (repeat '_') Array.//) .
   map (mapSnd (\n -> toEnum $ n + fromEnum '0'))


fgColor :: ANSI.Color -> String
fgColor c = ANSI.clr (ANSI.fg c)

highlightBars ::
   (Array.Ix row, Array.Ix col) =>
   [(row, (col, col))] ->
   Array.Array (row, col) [Char] -> Array.Array (row, col) [Char]
highlightBars bars arr =
   Array.accum
      (\str lr ->
         if lr then ANSI.cyanBg ++ str else str ++ ANSI.resetBg) arr $
   concatMap
      (\(row,(left,right)) -> [((row,left),True), ((row,right),False)]) bars

formatColored :: Set X -> Maybe Cell -> [Cell] -> String
formatColored set current =
   unlines . map (intercalate " ") . sliceVertical 9 . Array.elems .
   highlightBars
      (flip foldMap set $ \x ->
       case x of
         Pos _ _ -> []
         Row _ row -> [(row, (0,8))]
         Column _ col -> map (\row -> (row, (col,col))) [0..8]
         Square _ row3 col3 ->
            map (\row -> (row, (col3*3,col3*3+2))) [row3*3 .. row3*3+2]) .
   (Array.//
      maybe []
         (\(currentPos, currentSym) ->
            [(currentPos,
              fgColor ANSI.Blue ++ show currentSym ++ fgColor ANSI.Reset)])
         current) .
   (listArray ((0,0),(8,8)) (repeat "_") Array.//) .
   map (mapSnd show)


exampleHawiki1 :: [String]
exampleHawiki1 =
   "    6  8 " :
   " 2       " :
   "  1      " :
   " 7    1 2" :
   "5   3    " :
   "      4  " :
   "  42 1   " :
   "3  7  6  " :
   "       5 " :
   []

exampleRandom :: [String]
exampleRandom =
   " 2 8 9  5" :
   "    2    " :
   " 9       " :
   "  19    7" :
   "8   5   6" :
   "2 9  8 5 " :
   "  3      " :
   " 1    8  " :
   "    652 1" :
   []

stateFromString ::
   (ESC.Set set) => [Assign set] -> [String] -> ESC.State Cell set
stateFromString asgns css =
   foldl (flip ESC.updateState) (ESC.initState asgns) $
   do let asnMap = foldMap (\asn -> Map.singleton (ESC.label asn) asn) asgns
      (i,cs) <- zip [0..] css
      (j,c)  <- zip [0..] cs
      guard $ c/=' '
      return $
         Map.findWithDefault
            (error "coordinates not available")
            ((i,j), fromEnum c - fromEnum '0') asnMap


data Step a b c =
     Attempt a
   | Complete b
   | Fail c

data Choice = None | Unique | Multiple

indentTree ::
   ESC.Tree label set ->
   [([Int], Step ((Choice, set), label, [label]) [label] (set, [label]))]
indentTree =
   let go numbers labels tree =
         case tree of
            ESC.Leaf -> [(numbers, Complete labels)]
            ESC.Branch set subTrees ->
               case subTrees of
                  [(label,subTree)] ->
                     (numbers, Attempt ((Unique, set), label, label:labels)) :
                     go numbers (label:labels) subTree
                  [] -> [(numbers, Fail (set, labels))]
                  _ ->
                     concatMap
                        (\(k, (label,subTree)) ->
                           (k:numbers,
                            Attempt ((Multiple, set), label, label:labels)) :
                           go (k:numbers) (label:labels) subTree) $
                     zip [1 ..] subTrees
   in  go [] []

formatReason :: (Choice, Set X) -> String
formatReason (choice, set) =
   let uniqueStr =
         case choice of
            None -> "no possible"
            Unique -> "unique"
            Multiple -> "try"
   in  case Set.toList set of
         [x] ->
            case x of
               Pos row col ->
                  printf "%s number at position (%i,%i)" uniqueStr row col
               Row k row ->
                  printf "%s position of %i in row %i" uniqueStr k row
               Column k col ->
                  printf "%s position of %i in column %i" uniqueStr k col
               Square k row3 col3 ->
                  printf "%s position of %i in square (%i,%i)"
                     uniqueStr k row3 col3
         _ -> error "reason set must be a singleton"


{- |
This generates lots of Sudoku puzzles in a random way.
However, I assume that it will not generate all possible sudokus,
it may generate duplicates and they will certainly not be equally distributed.
-}
randomPuzzles :: MS.State StdGen [[Cell]]
randomPuzzles =
   return . ESC.partitions
      =<< Random.intSetFromSetAssigns
      =<< Random.shuffle assigns

minimizePuzzle :: [Cell] -> [Cell]
minimizePuzzle =
   let asnMap = foldMap (\asn -> Map.singleton (ESC.label asn) asn) bitAssigns
       lookupAssign =
         flip (Map.findWithDefault (error "coordinates not available")) asnMap
       go state xs (y:ys) =
         case ESC.search $ foldl (flip ESC.updateState) state ys of
            [_] -> go state xs ys
            _ -> go (ESC.updateState y state) (ESC.label y : xs) ys
       go _ xs [] = xs
   in  go (ESC.initState bitAssigns) [] . map lookupAssign


main, mainAll, mainSolve, mainBit, mainBitVector, mainIntSet,
   mainTree, mainDetail, mainGenerate :: IO ()

mainAll =
   mapM_ (putStrLn . format) $ ESC.partitions bitAssigns

mainSolve =
   mapM_ (putStrLn . format) $ ESC.search $
   stateFromString assigns exampleHawiki1

mainBit =
   mapM_ (putStrLn . format) $ ESC.search $
   stateFromString bitAssigns exampleHawiki1

mainBitVector =
   mapM_ (putStrLn . format) $ ESC.search $
   stateFromString bitVectorAssigns exampleHawiki1

mainIntSet =
   mapM_ (putStrLn . format) $ ESC.search $
   stateFromString intSetAssigns exampleHawiki1

mainTree = do
   let s0 = stateFromString assigns exampleHawiki1
   forM_ (indentTree $ ESC.completeTree s0) $ \(numbers, msg) ->
      putStrLn $
         (intercalate "." $ map show $ reverse numbers)
         ++
         (case msg of
            Attempt (reason,(pos,k),_) ->
               ": " ++ show k ++ " at " ++ show pos ++
               " - " ++ formatReason reason
            Complete labels -> "\n" ++ format (labels ++ ESC.usedSubsets s0)
            Fail (reason,_) ->
               ": failed because " ++ formatReason (None,reason))

mainDetail = do
   let s0 = stateFromString assigns exampleHawiki1
   forM_ (indentTree $ ESC.completeTree s0) $ \(numbers, msg) ->
      putStrLn $
         (intercalate "." $ map show $ reverse numbers)
         ++
         (case msg of
            Attempt (reason, cell@(pos,k), labels) ->
               ": " ++ show k ++ " at " ++ show pos ++
               " - " ++ formatReason reason ++ "\n\n" ++
               formatColored (snd reason)
                  (Just cell) (labels ++ ESC.usedSubsets s0)
            Complete _labels -> ": completed\n"
            Fail (reason, labels) ->
               ": failed because " ++ formatReason (None,reason) ++ "\n\n" ++
               formatColored reason Nothing (labels ++ ESC.usedSubsets s0))

mainGenerate = do
   gen <- getStdGen
   case MS.evalState randomPuzzles gen of
      solution:_ -> do
         putStrLn $ formatSparse solution
         let minSolution = minimizePuzzle solution
         putStrLn $ formatSparse minSolution
         printf "%d initially filled cells\n\n" $ length minSolution
      _ -> fail "to few puzzles"

main = mainDetail
