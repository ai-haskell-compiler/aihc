module Combinatorics.Battleship where

import Data.Map (Map, )
import Data.Set (Set, )


type ShipSize = Int
type NumberOfShips = Int
type Fleet = Map ShipSize NumberOfShips


data Orientation = Horizontal | Vertical
   deriving (Show, Eq, Ord)

data Ship = Ship ShipSize Orientation (Int, Int)
   deriving (Show, Eq, Ord)

data Board = Board (Int, Int) (Set (Int, Int))
