{- |
Number of possible games as described in
<http://projecteuler.net/problem=306>.
-}
module Combinatorics.PaperStripGame (
   numbersOfGames,
   numbersOfGamesSeries,
   treeOfGames,
   ) where

import qualified Combinatorics as Combi
import qualified PowerSeries as PS
import qualified Data.List.HT as ListHT
import qualified Data.Tree as Tree
import Data.Tree (Tree, )
import Data.List (inits, tails, )
import Control.Monad (guard, )


{-
representation:
store the original position of every box
-}
_cutEverywhere0 :: [Int] -> [[Int]]
_cutEverywhere0 xs = do
   (ys, z0:z1:zs) <- zip (inits xs) (tails xs)
   guard $ succ z0 == z1
   return $ ys++zs

{-
representation:
list the sizes of the parts

cutEverywhere1 [10] ~ cutEverywhere [0..9]
cutEverywhere1 [2,5] ~ cutEverywhere [0,1,3,4,5,6,7]
                  or   cutEverywhere [0,1,4,5,6,7,8]
-}
cutEverywhere1 :: [Int] -> [[Int]]
cutEverywhere1 zs = do
   (xs,n,ys) <- ListHT.splitEverywhere zs
   (a,b) <- cutPart n
   return $ xs ++ filter (0/=) [a,b] ++ ys

cutPart :: Int -> [(Int, Int)]
cutPart n =
   zip [0..] $ takeWhile (>=0) $ iterate pred (n-2)

treeOfGames :: Int -> Tree [Int]
treeOfGames n =
   Tree.unfoldTree (\ns -> (ns, if null ns then [] else cutEverywhere1 ns)) [n]

lengthOfGames :: Int -> [Int]
lengthOfGames =
   let go n ls =
          if all (<=1) ls
            then [n]
            else concatMap (go (succ n)) $ cutEverywhere1 ls
   in  go 0 . (:[])

{-
[1,1,1,2,3,6,12,26,60,144,366,960,2640,7464,21960,66240,206760,660240,2172240,7298640,...
-}
numbersOfGames :: [Int]
numbersOfGames =
   map (length . lengthOfGames) [0..]

{-
directions:
  number of boxes ->
  length of game v

That is, the k-th column contains the histogram of (lengthOfGames n).

  |  0   1   2   3   4   5   6   7   8   9  10
----------------------------------------------
0 |  1   1
1 |          1   2   1
2 |                  2   6   6   2
3 |                          6  24  36  24   6
4 |                                 24 120 240
5 |                                        120


a_n_k = binomial (n+1) (k-2*n) * factorial k
-}


numbersOfGamesSeries :: [Integer]
numbersOfGamesSeries =
   foldr (\(x0:x1:xs) ys -> x0 : x1 : PS.add xs ys) [] $
   zipWith PS.scale Combi.factorials $ tail Combi.binomials
