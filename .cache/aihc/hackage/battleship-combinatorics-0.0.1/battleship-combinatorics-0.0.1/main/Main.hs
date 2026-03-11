module Main where

import qualified Combinatorics.Battleship.SetCover as SetCover
import qualified Combinatorics.Battleship.Count.ShortenShip.Distribution as
                                                                   Distribution
import qualified Combinatorics.Battleship.Count.ShortenShip as ShortenShip
import qualified Combinatorics.Battleship.Enumeration as Enumeration
import qualified Combinatorics.Battleship.Fleet as Fleet
import Combinatorics.Battleship.Size (n6)

import qualified Data.Map as Map


main :: IO ()
main =
   case fromInteger 20 :: Int of
      00 -> Enumeration.count8x8
      01 -> Enumeration.count (10,10) (Map.fromList [(2,1),(4,1),(5,1)])
      10 -> SetCover.estimateDistribution
      11 -> ShortenShip.printMapSizes
      12 -> ShortenShip.countExternal
      13 -> ShortenShip.count8x8
      14 -> print $ ShortenShip.count (n6,6) $ Fleet.fromList [(2,2), (3,2)]
      15 -> ShortenShip.countFleets
      20 -> Distribution.countExternal
      21 -> Distribution.countExternal >> SetCover.exactDistribution
      _ -> return ()
