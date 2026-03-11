{- |
Enumerate all possible configurations in the Battleship game.
-}
module Combinatorics.Battleship.Enumeration where

import Combinatorics.Battleship
         (Fleet, ShipSize, Orientation(..), Ship(Ship), Board(Board), )
import Combinatorics (tuples)

import Data.Map (Map, )
import Data.Set (Set, )
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Class as MT
import Control.Monad (liftM2, guard, when, )
import Data.List.HT (tails, )
import Data.Bool.HT (if', )

import qualified System.IO as IO


insertShip :: Ship -> Board -> Board
insertShip ship (Board bnds set) =
   Board bnds $ Set.union set $ shipArea ship

shipArea :: Ship -> Set (Int, Int)
shipArea (Ship size orient (x,y)) =
   Set.fromAscList $
      case orient of
         Horizontal -> map (flip (,) y) [x .. x+size-1]
         Vertical -> map ((,) x) [y .. y+size-1]

reduceSpace :: Ship -> Board -> Board
reduceSpace ship (Board bnds set) =
   Board bnds $
   Set.difference set $
   shipOutline ship

shipOutline :: Ship -> Set (Int, Int)
shipOutline (Ship size orient (x,y)) =
   Set.fromAscList $
      case orient of
         Horizontal -> liftM2 (,) [x-1 .. x+size] [y-1 .. y+1]
         Vertical -> liftM2 (,) [x-1 .. x+1] [y-1 .. y+size]


data Box = Box (Int, Int) (Int, Int)

shipBounds :: Ship -> Box
shipBounds (Ship size orient (x,y)) =
   case orient of
      Horizontal -> Box (x,y) (x+size-1, y)
      Vertical -> Box (x,y) (x, y+size-1)

moveShip :: (Int, Int) -> Ship -> Ship
moveShip (dx,dy) (Ship size orient (x,y)) =
   Ship size orient (x+dx, y+dy)

{- |
Bounding box around two boxes.
-}
mergeBox :: Box -> Box -> Box
mergeBox (Box (a0x,a0y) (a1x,a1y)) (Box (b0x,b0y) (b1x,b1y)) =
   Box (min a0x b0x, min a0y b0y) (max a1x b1x, max a1y b1y)

{- |
Intersection of two boxes.
If the intersection is empty,
then the box will have left and right boundaries
or upper and lower boundaries in swapped order.
-}
intersectBox :: Box -> Box -> Box
intersectBox (Box (a0x,a0y) (a1x,a1y)) (Box (b0x,b0y) (b1x,b1y)) =
   Box (max a0x b0x, max a0y b0y) (min a1x b1x, min a1y b1y)

boxSizes :: Box -> (Int, Int)
boxSizes (Box (a0x,a0y) (a1x,a1y)) = (a1x - a0x + 1, a1y - a0y + 1)


emptyBoard :: (Int, Int) -> Board
emptyBoard bnds = Board bnds Set.empty

fullBoard :: (Int, Int) -> Board
fullBoard bnds@(width,height) =
   Board bnds $ Set.fromAscList $
   liftM2 (,) [0 .. width-1] [0 .. height-1]

boardFromShips :: (Int, Int) -> [Ship] -> Board
boardFromShips bnds =
   foldl (flip insertShip) (emptyBoard bnds)

formatBoard :: Board -> String
formatBoard (Board (width,height) set) =
   unlines $
   map
      (\y ->
         map
            (\x -> if Set.member (x,y) set then 'x' else '.')
            [0 .. width-1])
      [0 .. height-1]

charmapFromShip :: Ship -> Map (Int, Int) Char
charmapFromShip (Ship size orient (x,y)) =
   Map.fromAscList $
      case orient of
         Horizontal ->
            ((x,y), '<') :
            map (\k -> ((k,y), '-')) [x+1 .. x+size-2] ++
            ((x+size-1,y), '>') :
            []
         Vertical ->
            ((x,y), 'A') :
            map (\k -> ((x,k), '|')) [y+1 .. y+size-2] ++
            ((x,y+size-1), 'V') :
            []

formatShips :: (Int, Int) -> [Ship] -> String
formatShips (width,height) ships =
   let charMap = Map.unions $ map charmapFromShip ships
   in  unlines $
       map
          (\y ->
             map
                (\x -> Map.findWithDefault '.' (x,y) charMap)
                [0 .. width-1])
          [0 .. height-1]


tryShip ::
   Bool -> Ship -> MS.StateT (Set (Int,Int)) [] Ship
tryShip outline ship = do
   guard =<< MS.gets (Set.isSubsetOf (shipArea ship))
   MS.modify (flip Set.difference (if' outline shipOutline shipArea ship))
   return ship


tryShipsOfOneSize ::
   Bool -> Int -> Int ->
   MS.StateT (Set (Int,Int)) [] [Ship]
tryShipsOfOneSize outline size number =
   mapM (tryShip outline . uncurry (Ship size))
    =<< MT.lift
    =<< MS.gets (tuples number . liftM2 (,) [Vertical, Horizontal] . Set.toList)


fleetFromSizes :: [ShipSize] -> Fleet
fleetFromSizes = Map.fromListWith (+) . map (flip (,) 1)

standardFleet :: Fleet
standardFleet = Map.fromList [(5,1), (4,2), (3,3), (2,4)]

configurationsInFragment :: Bool -> Fleet -> Set (Int,Int) -> [[Ship]]
configurationsInFragment outline fleet set =
   MS.evalStateT
      (fmap concat $
       mapM (uncurry (tryShipsOfOneSize outline)) $
       Map.toDescList fleet)
      set

{-
Enumerate all possible configurations in the Battleship game.
-}
configurations :: (Int,Int) -> Fleet -> [[Ship]]
configurations bnds fleet =
   configurationsInFragment True fleet $
      case fullBoard bnds of Board _ set -> set

configurationsTouching :: (Int,Int) -> Fleet -> [[Ship]]
configurationsTouching bnds fleet =
   configurationsInFragment False fleet $
      case fullBoard bnds of Board _ set -> set

{-
*Combinatorics.Battleship.Enumeration> length $ configurations (9,9) (Map.fromList [(5,1)])
90
*Combinatorics.Battleship.Enumeration> length $ configurations (9,9) (Map.fromList [(4,2)])
3826
*Combinatorics.Battleship.Enumeration> length $ configurations (9,9) (Map.fromList [(3,3)])
134436
*Combinatorics.Battleship.Enumeration> length $ configurations (9,9) (Map.fromList [(2,4)])
5534214

*Combinatorics.Battleship.Enumeration> length $ configurations (10,10) (Map.fromList [(5,1)])
120
*Combinatorics.Battleship.Enumeration> length $ configurations (10,10) (Map.fromList [(4,2)])
6996
*Combinatorics.Battleship.Enumeration> length $ configurations (10,10) (Map.fromList [(3,3)])
330840
*Combinatorics.Battleship.Enumeration> length $ configurations (10,10) (Map.fromList [(2,4)])
17086631

*Combinatorics.Battleship.Enumeration> length $ configurations (10,10) (Map.fromList [(5,1),(4,2)])
371048
*Combinatorics.Battleship.Enumeration> length $ configurations (10,10) (Map.fromList [(5,1),(3,3)])
13477504
-}


enumerateStandard :: IO ()
enumerateStandard =
   let bnds = (10, 10)
   in  mapM_ (putStrLn . formatShips bnds) $
       take 100 $
       configurations bnds standardFleet


{- |
<http://math.stackexchange.com/questions/58769/how-many-ways-can-we-place-these-ships-on-this-board>
-}
count :: (Int,Int) -> Fleet -> IO ()
count bnds fleet =
       do IO.hSetBuffering IO.stdout IO.LineBuffering
          mapM_
             (\(n,configs) ->
                case configs of
                   [] -> putStrLn $ "number of configurations: " ++ show (n::Integer)
                   (c:_) ->
                      when (mod n 1000000 == 0) $ do
                         print n
                         putStrLn ""
                         putStrLn $ formatShips bnds c) $
             zip [0..] $ tails $
             configurationsTouching bnds fleet

count8x8 :: IO ()
count8x8 = count (8, 8) (Map.fromList [(2,1), (3,2), (4,1), (5,1)])
{-
non-touching:
16546192


touching:
571126760

time required for computation:
real    41m36.880s
user    41m23.183s
sys     0m8.681s
-}

main :: IO ()
main = count8x8
