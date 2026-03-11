{- |
The module provides a pretty naive encoding for a nonogram.
Black squares are encoded as elements of bricks by 'assignsFromBrick'.
The board is filled with white squares
when there are no more black squares to set.
This is declared in the 'liftM2' part in 'assigns'.
This part is also responsible for connecting
the horizontally and the vertically sliced view of the board.

This approach has two main problems:

* Bricks are only set as a whole.
  For an efficient solution it would be necessary
  to infer the color of single squares.

* White is encoded as \"finally not black\".
  An efficient solution requires three states for a square:
  \"black\", \"white\", \"yet undetermined\".

The modules "Nonogram.Encoding.BlackWhite"
and "Nonogram.Encoding.Plug" address these problems.
-}
module Nonogram.Encoding.Naive (assigns) where

import Nonogram.Base
         (Strip(Strip), strip, BrickId(BrickId),
          Orientation(Horizontal, Vertical), noAssign)

import qualified Math.SetCover.Exact as ESC

import Control.Monad (liftM2)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.List.Match as Match
import qualified Data.List as List
import Data.Foldable (foldMap)


{- |
'Reserve' ensures that @Brick n@ is left from @Brick (n+1)@.
@Brick n@ forbids @Brick (n+1)@ to use any squares
from the left border to its right end,
and @Brick (n+1)@ forbids @Brick n@ to use any squares
from its left start to right border.
Gaps are filled with single 'Reserve' items in 'assignsFromLine'.
-}
data Item = Brick BrickId | Position Int | Reserve BrickId Int
   deriving (Eq, Ord, Show)


type Assign = ESC.Assign (Set (Int, Int)) (Map Strip (Set Item))

assignsFromBrick ::
   Orientation -> Int -> Int ->
   Maybe BrickId -> BrickId -> Maybe BrickId -> Int -> [Assign]
assignsFromBrick orient width line prevBrick thisBrick maybeThisBrick size =
   flip map [0 .. width-size] $ \col ->
   ESC.assign
      (case orient of
         Horizontal -> Set.fromList $ take size $ map ((,) line) [col ..]
         Vertical -> Set.empty) $
   strip orient line $
   Brick thisBrick
   :
   (map Position $ take size [col ..])
   ++
   foldMap
      (\brick -> map (Reserve brick) [col .. pred width])
      prevBrick
   ++
   foldMap
      (\brick -> map (Reserve brick) [0 .. min (pred width) (col+size)])
      maybeThisBrick

assignsFromLine ::
   Orientation -> Int -> Int -> [Int] -> [Assign]
assignsFromLine orient width line xs =
--   let bricks = Match.take (ListHT.laxTail xs) [BrickId 0 ..]
   let bricks = Match.take (drop 1 xs) [BrickId 0 ..]
   in  concat
          (List.zipWith4
             (assignsFromBrick orient width line)
             (Nothing : map Just bricks)
             [BrickId 0 ..]
             (map Just bricks ++ [Nothing])
             xs)
       ++
       liftM2
          (\brick c -> noAssign $ strip orient line [Reserve brick c])
          bricks [0 .. width-1]

assigns :: [[Int]] -> [[Int]] -> [Assign]
assigns rows columns =
   concat (zipWith (assignsFromLine Horizontal (length columns)) [0..] rows)
   ++
   concat (zipWith (assignsFromLine Vertical (length rows)) [0..] columns)
   ++
   liftM2
      (\r c ->
         noAssign $
         Map.fromList
            [(Strip Horizontal r, Set.singleton (Position c)),
             (Strip Vertical c, Set.singleton (Position r))])
      (Match.take rows [0..])
      (Match.take columns [0..])
