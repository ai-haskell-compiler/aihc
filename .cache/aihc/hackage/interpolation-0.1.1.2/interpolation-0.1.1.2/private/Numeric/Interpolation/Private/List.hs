module Numeric.Interpolation.Private.List where

import qualified Data.List as List


mapAdjacent3 :: (a -> a -> a -> b) -> [a] -> [b]
mapAdjacent3 f xs0 =
   let xs1 = drop 1 xs0
       xs2 = drop 1 xs1
   in  List.zipWith3 f xs0 xs1 xs2

mapAdjacentMaybe3 :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
mapAdjacentMaybe3 f xs =
   let jxs = map Just xs
   in  zipWith3 f (Nothing : jxs) xs (drop 1 jxs ++ [Nothing])

mapAdjacentMaybe5 ::
   (Maybe a -> Maybe a -> a -> Maybe a -> Maybe a -> b) ->
   [a] -> [b]
mapAdjacentMaybe5 f xs =
   let jxs = map Just xs
       lxs1 = Nothing : jxs
       lxs2 = Nothing : lxs1
       rxs1 = drop 1 $ jxs ++ repeat Nothing
       rxs2 = drop 1 $ rxs1
   in  List.zipWith5 f lxs2 lxs1 xs rxs1 rxs2

