module Combinatorics.Battleship.Count.Estimate (
   overlapping,
   occupying,
   ) where

import qualified Combinatorics.Battleship.Fleet as Fleet

import qualified Data.NonEmpty as NonEmpty


overlapping :: (Int,Int) -> Fleet.T -> Integer
overlapping (width,height) =
   product .
   map
      (\(size,count) ->
         (fromIntegral height * fromIntegral (max 0 (width-size+1)) +
          fromIntegral width * fromIntegral (max 0 (height-size+1)))
            ^ count) .
   Fleet.toList


{-
This takes into account
that every ship occupies spaces that cannot be used for other ships anymore.
We reduce the available area ship by ship
and then estimate the number of remaining positions for each ship.
Unfortunately, we do not know the shape of the area -
it depends on the position of the placed ships.
We work-around this problem by selecting rectangles
that have at least the required area.
This leads to an upper bound, given that a shape of a certain area
provides a maximum of ship positions if it is rectangular.
-}
occupying :: (Int,Int) -> Fleet.T -> Integer
occupying (width,height) fleet =
   let sizes = reverse $ Fleet.toSizes fleet
   in  product $ map toInteger $
       zipWith (flip $ maxPositionsInArea (width+1,height+1)) sizes $
       scanl (-) ((width+1)*(height+1)) $
       map (\size -> 2*(fromIntegral size + 1)) sizes

maxPositionsInArea :: (Int,Int) -> Int -> Int -> Int
maxPositionsInArea (maxWidth,maxHeight) area size =
   NonEmpty.maximum $ NonEmpty.cons 0 $
   concatMap
      (\(width,height) -> [(width-size)*(height-1), (width-1)*(height-size)]) $
   filter
      (\(width,height) ->
         width<=maxWidth && height<=maxHeight
         ||
         height<=maxWidth && width<=maxHeight) $
   rectangles area

rectangles :: Int -> [(Int,Int)]
rectangles area =
   takeWhile (uncurry (<=)) $ map (\a -> (a, divUp area a)) [2..]

-- cf. numeric-prelude:Algebra.IntegralDomain.divUp
divUp :: (Integral a) => a -> a -> a
divUp n m = - div (-n) m
