{- |
In this approach we try to count the number of battleship configurations
pushing a diagonal frontier over the board.
The frontier is not exactly diagonal but monotonic.


Attention!

This counting approach counts configurations twice.
The smallest known of such configurations is:

..*
...
*..


However, you can use this counting for getting an upper bound.
If the upper bound is zero, then there is no possibility to layout the ships.
This way we can prove,
that the 8x8 area cannot be filled with the fleet of 10x10 board game.
-}

{-
possible optimizations:

 - do not cache, but compute counts for single ships
 - do not cache count for empty fleets
 - use bitvector for Frontier
 - use bitvector for Fleet
-}

module Combinatorics.Battleship.Count.DiagonalFrontier where

import Combinatorics.Battleship.Enumeration (configurationsInFragment, )
import Combinatorics.Battleship (Fleet, )

import Data.Map (Map, )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (guard, )
import Data.Traversable (forM, )

import Data.List.HT (mapAdjacent, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, )


{-
(map fst frontier) must be strictly increasing
and (map snd frontier) must be strictly decreasing.

E.g. the following area

  xxxxxxx...
  xxxxxxx...
  xxx.......
  xxx.......
  xxx.......
  xxx.......
  xx........
  xx........
  xx........
  xx........

is represented by the frontier

  [(2,6), (3,2), (7,0)]

.

We could represent a frontier more efficiently by a bit vector
that tells whether an edge (of one square) in the frontier
is vertical or horizontal.
For the example above we would get the sequence:

  hhvvvvhvvvvhhhhvvhhh
-}
type Frontier = [(Int, Int)]

areaWithinFrontier :: (Int, Int) -> Frontier -> Int
areaWithinFrontier (width, height) corners =
   sum $
   zipWith (*)
      (map ((width - ) . fst) corners)
      (mapAdjacent (-) $ height : map snd corners)

borderFromFrontier :: Frontier -> Frontier
borderFromFrontier =
   map (\(x,y) -> (x-1,y-1))

{-
It must hold:
areaWithinFrontier bnds frontier ==
   length (positionsWithinFrontier bnds frontier)
-}
positionsWithinFrontier :: (Int, Int) -> Frontier -> [(Int,Int)]
positionsWithinFrontier (width, height) corners = do
   (x0,(y1,y0)) <-
      zipWith (,)
         (map fst corners)
         (mapAdjacent (,) $ height : map snd corners)
   x <- [x0 .. width-1]
   y <- [y0 .. y1-1]
   return (x,y)

clipBottomFrontier :: Int -> Frontier -> Frontier
clipBottomFrontier height xys =
   dropWhile ((height<) . snd) xys

clipRightFrontier :: Int -> Frontier -> Frontier
clipRightFrontier width xys =
   takeWhile ((<=width) . fst) xys

clipBottomFrontier0 :: Int -> Frontier -> Frontier
clipBottomFrontier0 height xys =
   dropWhile ((height<=) . snd) xys

clipRightFrontier0 :: Int -> Frontier -> Frontier
clipRightFrontier0 width xys =
   takeWhile ((<width) . fst) xys

{-
clipBottomFrontier :: Int -> Frontier -> Frontier
clipBottomFrontier height xys =
   let (left, right) = span ((height<=) . snd) xys
   in  if null left
         then right
         else (fst $ last left, height) : right

clipRightFrontier :: Int -> Frontier -> Frontier
clipRightFrontier width xys =
   let (lower, upper) = span ((<width) . snd) xys
   in  lower ++
       if null upper
         then []
         else [(width, snd $ head upper)]
-}

cornersForShip :: (Int, Int) -> Frontier -> Int -> [(Int, Int)]
cornersForShip (width, height) frontier shipSize =
   (map (\(x,y) -> (x+2, y+shipSize+1)) $
    positionsWithinFrontier (width, height-shipSize+1) $
    clipBottomFrontier (height-shipSize) frontier)
   ++
   (map (\(x,y) -> (x+shipSize+1, y+2)) $
    positionsWithinFrontier (width-shipSize+1, height) $
    clipRightFrontier (width-shipSize) frontier)

{-
Reduce area in frontier by cutting away a top-left aligned rectangle
specified by its width and height.
-}
moveFrontier :: (Int, Int) -> Frontier -> Frontier
moveFrontier (cx,cy) xys =
   let (left, right) = span ((cy<) . snd) xys
       (lower, upper) = span ((<=cx) . fst) right
   in  normalizeFrontier $
       left ++
       (if null lower
          then []
          else [(fst $ head lower, cy), (cx, snd $ last lower)]) ++
       upper

normalizeFrontier :: Frontier -> Frontier
normalizeFrontier ((x0,y0):xys0@((x1,y1):xys1)) =
   if x0 == x1
     then normalizeFrontier xys0
     else
       if y0 == y1
         then normalizeFrontier $ (x0,y0) : xys1
         else (x0,y0) : normalizeFrontier xys0
normalizeFrontier xys = xys

{-
This works for a frontier representation by bottom-right corners.

moveFrontier :: (Int, Int) -> Frontier -> Frontier
moveFrontier cxy@(cx,cy) xys =
   let (left, right) = span ((cy<=) . snd) xys
       (lower, upper) = span ((cx>=) . fst) right
   in  left ++ (if null lower then [] else [cxy]) ++ upper
-}

{-
It holds @length (allFrontiers (w,h)) + 1 == binomial (w+h) h@.
We omit the frontier that has no area.

If we could generate the frontiers in a lexicographically sorted way,
then we could use an efficient Map.fromAscList.
-}
allFrontiers :: (Int, Int) -> [Frontier]
allFrontiers (width, height) = do
   x <- [0 .. width-1]
   y <- [0 .. height-1]
   allFrontiersAt (x,y) (width-x, height-y)

allFrontiersAt :: (Int, Int) -> (Int, Int) -> [Frontier]
allFrontiersAt (x,y) (width, height) =
   [(x,y)] : do
      dx <- [1 .. width-1]
      dy <- [1 .. height-1]
      poss <- allFrontiersAt (x+dx,y) (width-dx, dy)
      return $ (x,y+dy) : poss


minimumAreaForFleet :: Fleet -> Int
minimumAreaForFleet =
   sum . map (\(size,num) -> (size+1)*2*num) . Map.toList


countPartial ::
   Map (Frontier, Fleet) Integer ->
   (Int, Int) -> Frontier -> Fleet -> Integer
countPartial cnts bnds@(width,height) frontier fleet =
   if Map.null fleet
     then 1
     else sum $ do
        shipSize <- Map.keys fleet
        let restFleet =
               Map.update
                  (\n0 -> let n1 = n0-1 in toMaybe (n1>0) n1)
                  shipSize fleet
        newCorner <-
           cornersForShip bnds frontier shipSize
        return $
           fromMaybe 0 $
           Map.lookup
              (clipBottomFrontier0 height $
               clipRightFrontier0 width $ 
               moveFrontier newCorner frontier,
               restFleet)
              cnts

countAll :: (Int, Int) -> Fleet -> Map (Frontier, Fleet) Integer
countAll bnds fleet =
   let cnts =
          Map.fromList $ do
             frontier <- [] : allFrontiers bnds
             let freeArea =
                    areaWithinFrontier bnds $
                    borderFromFrontier frontier
             partialFleet0 <-
                forM fleet $ \num -> [0 .. num]
             let partialFleet = Map.filter (0/=) partialFleet0
             guard $ minimumAreaForFleet partialFleet <= freeArea
             return ((frontier, partialFleet),
                     countPartial cnts bnds frontier partialFleet)
   in  cnts

count :: (Int, Int) -> Fleet -> Integer
count bnds fleet =
   Map.findWithDefault
      (error "count: did not find largest frontier")
      ([(0,0)], fleet) $
   countAll bnds fleet




testCountAll ::
   (Int, Int) -> Fleet -> Map (Frontier, Fleet) (Integer, Integer)
testCountAll bnds =
   Map.filter (uncurry (/=)) .
   Map.mapWithKey (\(frontier, fleet) cnt ->
      (cnt,
       fromIntegral $ length $ configurationsInFragment True fleet $
       Set.fromList $ positionsWithinFrontier bnds frontier)) .
   countAll bnds


{-
other ideas for counting:

- make an induction over the ship size and count layouts in connected areas
   i.e. place the 5-size-ship somewhere,
        divide the remaining free space into connected components,
        count the number of layouts of the [4:2, 3:3, 2:4] fleet
        for all of these components.
        For all possible component shapes place the two 4-size ships
        and divide the remaining space into connected components.
        And so on, and so on.
   In order to be efficient we need an efficient map
   from component shapes to numbers.
   We could represent a component shape using a bit vector,
   and use this bit vector as key of a Map.
   But it is certainly also a good idea to avoid rebalancing.
   Can we find a lazy trie structure, that saves enough space
   by not evaluating impossible component shapes?
   We could use the outline of the shape, but a component may contain holes.

- Sort the ships lexicographically according to their left-top corner.
  Try all positions for the lexicographically first ship.
  Maintain a frontier
  that is a horizontal sequence of heights of free space.
  E.g. the following area

    xxxxxx..xx
    xxxx....xx
    .xxx....xx
    ..........
    ..........

  is represented by the frontier

    [3,2,2,2,4,4,5,5,2,2]

  The frontier itself has a maximum height of 6,
  since ships with lexicographically smaller positions
  than the already layouted ones are not possible
  and maximum size of a ship is 5.
  The maximum number of frontiers is thus 5*6^10.
  There are less such frontiers,
  since not all jump heights in such a frontier are possible.
  We might employ a specialised Trie
  that saves memory by not evaluating certain frontiers.
  The key of the trie consists of the fleet size, the frontier,
  and the lexicographically next position.
  The next position can be merged into the frontier,
  by removing a single square from the free space.
  The keys in the trie should somehow combine the fleet size
  and the frontier height,
  since for small frontier heights and big fleets
  the number of layouts is zero and we do not need to store that.
-}
