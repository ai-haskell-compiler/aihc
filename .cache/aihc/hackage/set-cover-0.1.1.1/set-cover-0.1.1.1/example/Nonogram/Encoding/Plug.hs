{- |
This module provides an alternative to "Nonogram.Encoding.BlackWhite"
but is not as efficient.

We use a different scheme for asserting
that the bricks are ordered from left to right.
For every (horizontal) strip imagine an array of squares,
where the horizontal axis is equal to the width of the board
and the vertical axis counts the bricks interleaved with spaces.
I.e. first row equals the left-most space,
second row to the left-most brick,
third row to the second space,
fourth row to the second brick, and so on.
A white square within the n-th space (zero-based) at position k
is represented by a vertical strip at position k,
where the square at the 2n-th line is moved one place to the right.
The n-th brick is represented by a block
where at the left edge the square in the 2n-1-th line is omitted
and right of the right edge a square is added in the 2n+1-th line.
We can plug together these shapes
which was the inspiration for the module name.
The plugs somehow form a staircase.

Unfortunately, this encoding does not improve performance of the solver.
I think it makes it more difficult for the solver
to see admissible positions for a brick
after some bricks have been placed.
-}
module Nonogram.Encoding.Plug (assigns) where

import qualified Nonogram.Base as Base
import Nonogram.Base
         (Strip, strip, BrickId(BrickId),
          Orientation(Horizontal, Vertical), Color(White, Black), noAssign)

import qualified Math.SetCover.Exact as ESC

import Control.Monad (liftM2)

import qualified Data.Monoid.HT as Mn
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Map (Map)
import Data.Set (Set)


data Item = Brick BrickId | Position Int Color | Square (BrickId, Color) Int
   deriving (Eq, Ord, Show)

instance Base.Position Item where position = Position


type Assign = ESC.Assign (Set (Int, Int)) (Map Strip (Set Item))

invertColor :: Color -> Color
invertColor c =
   case c of
      Black -> White
      White -> Black

square :: (BrickId, Color) -> Int -> [Item]
square cb k =
   Position k (invertColor $ snd cb) :
   Square cb k :
   []

assignsFromBrick ::
   Orientation -> Int -> Int ->
   [(BrickId, Color)] -> BrickId -> Int -> [Assign]
assignsFromBrick orient width line colorBricks thisBrick size =
   flip map [0 .. width-size] $ \col ->
   noAssign $
   strip orient line $
   Brick thisBrick
   :
   Mn.when (col>0) (square (thisBrick, White) (pred col))
   ++
   let cols = take size [col ..]
   in  map (flip Position (invertColor Black)) cols
       ++
       liftM2 Square colorBricks cols
   ++
   Mn.when (col+size<width)
      (map (flip Square (col+size)) $
       List.delete (succ thisBrick, White) colorBricks)

assignsFromLine ::
   Orientation -> Int -> Int -> [Int] -> [Assign]
assignsFromLine orient width line xs =
   let colorBricks =
          Match.take (0:xs++xs) $ liftM2 (,) [BrickId 0 ..] [White, Black]
       cbsels = ListHT.removeEach colorBricks
   in  concat
          (List.zipWith
             (assignsFromBrick orient width line colorBricks)
             [BrickId 0 ..] xs)
       ++
       map
          (\(_cb,cbs) ->
             noAssign $ strip orient line $ map (flip Square 0) cbs)
          (take 1 cbsels)
       ++
       liftM2
          (\(cb,cbs) k ->
             noAssign $
             strip orient line $
                square cb k ++ map (flip Square (succ k)) cbs)
          (ListHT.sieve 2 cbsels) [0 .. width-2]
       ++
       map
          (\(cb,_cbs) ->
             noAssign $ strip orient line $ square cb (width-1))
          (ListHT.takeRev 1 cbsels)

assigns :: [[Int]] -> [[Int]] -> [Assign]
assigns rows columns =
   concat (zipWith (assignsFromLine Horizontal (length columns)) [0..] rows)
   ++
   concat (zipWith (assignsFromLine Vertical (length rows)) [0..] columns)
   ++
   Base.assignsFromPositions Base.square rows columns
