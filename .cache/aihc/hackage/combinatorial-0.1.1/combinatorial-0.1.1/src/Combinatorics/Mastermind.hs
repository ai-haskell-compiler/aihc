module Combinatorics.Mastermind (
   Eval(..),
   evaluate,
   evaluateAll,
   formatEvalHistogram,
   numberDistinct,
   -- * only for testing
   numberDistinctWhite,
   ) where

import qualified Combinatorics.Permutation.WithoutSomeFixpoints as PermWOFP
import Combinatorics (binomial)

import Text.Printf (printf)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapPair)


{- $setup
>>> import qualified Combinatorics.Mastermind as Mastermind
>>> import qualified Combinatorics as Comb
>>> import qualified Test.QuickCheck as QC
>>> import Control.Monad (replicateM)
>>> import Data.List (genericLength)
>>>
>>> genMastermindDistinct :: QC.Gen (Int, Int, Int, Int)
>>> genMastermindDistinct = do
>>>    n <- QC.choose (0,12)
>>>    k <- QC.choose (0, min 5 n)
>>>    b <- QC.choose (0,k)
>>>    w <- QC.choose (0,k-b)
>>>    return (n,k,b,w)
-}


{- |
Cf. @board-games@ package.
-}
data Eval = Eval {black, white :: Int}
   deriving (Eq, Ord, Show)

{- |
Given the code and a guess, compute the evaluation.

>>> filter ((Mastermind.Eval 2 0 ==) . Mastermind.evaluate "aabbb") $ replicateM 5 ['a'..'c']
["aaaaa","aaaac","aaaca","aaacc","aacaa","aacac","aacca","aaccc","acbcc","accbc","acccb","cabcc","cacbc","caccb","ccbbc","ccbcb","cccbb"]
-}
evaluate :: (Ord a) => [a] -> [a] -> Eval
evaluate code attempt =
   uncurry Eval $
   mapPair
      (length,
       Fold.sum . uncurry (Map.intersectionWith min) .
       mapPair (histogram,histogram) . unzip) $
   ListHT.partition (uncurry (==)) $
   zip code attempt

evaluateAll :: (Ord a) => [[a]] -> [a] -> Map Eval Int
evaluateAll codes attempt = histogram $ map (evaluate attempt) codes

formatEvalHistogram :: Map Eval Int -> String
formatEvalHistogram m =
   let n = maximum $ map (\(Eval b w) -> b+w) $ Map.keys m
   in  unlines $
       zipWith
          (\b ->
             unwords .
             map (\w -> printf "%6d" $ Map.findWithDefault 0 (Eval b w) m))
          [0..] (reverse $ tail $ ListHT.inits [0..n])


histogram :: (Ord a) => [a] -> Map a Int
histogram  =  Map.fromListWith (+) . map (\a -> (a,1))


{- |
@numberDistinct n k b w@ computes the number of matching codes,
given that all codes have distinct symbols.
@n@ is the alphabet size, @k@ the width of the code,
@b@ the number of black evaluation sticks and
@w@ the number of white evaluation sticks.

prop> QC.forAll genMastermindDistinct $ \(n,k,b,w) -> let alphabet = take n ['a'..]; code = take k alphabet in Mastermind.numberDistinct n k b w == (genericLength $ filter ((Mastermind.Eval b w ==) . Mastermind.evaluate code) $ Comb.variate k alphabet)
-}
numberDistinct :: Int -> Int -> Int -> Int -> Integer
numberDistinct n k b w =
   binomial (toInteger k) (toInteger b)
   *
   numberDistinctWhite (n-b) (k-b) w

{- |
prop> QC.forAll genMastermindDistinct $ \(n,k,_b,w) -> Mastermind.numberDistinctWhite n k w == Mastermind.numberDistinct n k 0 w
-}
numberDistinctWhite :: Int -> Int -> Int -> Integer
numberDistinctWhite n k w =
   let ni = toInteger n
       ki = toInteger k
       wi = toInteger w
   in  binomial ki wi * PermWOFP.numbers !! k !! w * binomial (ni-ki) (ki-wi)
