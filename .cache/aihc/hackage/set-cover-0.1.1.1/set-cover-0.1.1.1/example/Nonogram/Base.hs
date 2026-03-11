module Nonogram.Base where

import qualified Math.SetCover.Exact as ESC

import Control.Monad (liftM3)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.List.Match as Match
import qualified Data.Monoid.HT as Mn
import Data.Monoid (Monoid, mempty)


{- |
Identifies one row or column by its orientation and its position.
-}
data Strip = Strip Orientation Int
   deriving (Eq, Ord, Show)

{- |
A brick is a horizontal or vertical stripe of adjacent black squares
delimited by white squares.
We number them per line or column.
-}
newtype BrickId = BrickId Int
   deriving (Eq, Ord, Show)

instance Enum BrickId where
   fromEnum (BrickId n) = n
   toEnum = BrickId

data Orientation = Horizontal | Vertical
   deriving (Eq, Ord, Show)

data Color = White | Black
   deriving (Eq, Ord, Show, Enum)

type ColorMap = Map (Int,Int) Color


noAssign :: (Monoid map) => set -> ESC.Assign map set
noAssign = ESC.assign mempty

strip :: Ord item => Orientation -> Int -> [item] -> Map Strip (Set item)
strip orient line = Map.singleton (Strip orient line) . Set.fromList

class Ord item => Position item where
   position :: Int -> Color -> item

{- |
Assert that colors are consistent in slices of both orientations.
-}
assignsFromPositions ::
   (Position item) =>
   (Int -> Int -> Color -> map) ->
   [[Int]] -> [[Int]] -> [ESC.Assign map (Map Strip (Set item))]
assignsFromPositions squ rows columns =
   liftM3
      (\r c col ->
         ESC.assign (squ r c col) $
         Map.fromList
            [(Strip Horizontal r, Set.singleton (position c col)),
             (Strip Vertical c, Set.singleton (position r col))])
      (Match.take rows [0..])
      (Match.take columns [0..])
      [White, Black]


square :: Int -> Int -> Color -> Set (Int,Int)
square r c col = Mn.when (col==Black) $ Set.singleton (r,c)

squareBW :: Int -> Int -> Color -> ColorMap
squareBW r c = Map.singleton (r,c)
