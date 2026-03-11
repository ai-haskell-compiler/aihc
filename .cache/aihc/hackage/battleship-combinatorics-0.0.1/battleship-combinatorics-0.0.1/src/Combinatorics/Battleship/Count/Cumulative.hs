module Combinatorics.Battleship.Count.Cumulative where

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Map (Map, )

import Control.Monad (liftM2, )

import Text.Printf (printf, )


size :: Int
size = 10

{- |
If the map contains @n@ at position @(x,y)@
this means that there are @n@ possible arrangements
of a certain number of 2-ships,
where you can place another 2-ship beginning at position @(x,y)@.
-}
type Board = Map (Int, Int) Integer

init2 :: Board
init2 = Map.fromList $ do
   x <- [0 .. pred size]
   y <- [0 .. pred size]
   return $ ((x,y), if y<size-1 then 1 else 0)

formatBoard :: Board -> String
formatBoard board =
   unlines $
   map
      (\y ->
         concatMap
            (\x -> printf "%10d" $ board Map.! (x,y))
            [0 .. pred size])
      [0 .. pred size]

iter :: Board -> Board
iter board = foldl1 (Map.intersectionWith (+)) $ do
   x <- [0 .. pred size]
   y <- [0 .. pred size]
   let n = board Map.! (x,y)
   return $ fmap (n*) $ deleteFromBoard (x,y) board

deleteFromBoard :: (Int, Int) -> Board -> Board
deleteFromBoard (x,y) board =
   foldl (flip $ Map.adjust (const 0)) board $
   liftM2 (,) [x-1 .. x+1] [y-2 .. y+2]

{-
correct numbers found by exhaustive enumeration
for 2-ships all vertically oriented.

no. ships  no. placements

1               90  (* 1)
2             3504  (* 2)
3            77856  (* 6)
4          1097615  (*24)
-}

main :: IO ()
main = do
   print $ Fold.sum init2 -- correct
   print $ Fold.sum $ iter init2 -- correct
   print $ Fold.sum $ iter $ iter init2 -- too big
