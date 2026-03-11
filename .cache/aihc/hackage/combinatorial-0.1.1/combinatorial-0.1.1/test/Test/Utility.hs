module Test.Utility where

import qualified Data.List as List
import Data.Eq.HT (equating)


equalFuncList :: (Integer -> Integer) -> [Integer] -> Int -> Bool
equalFuncList f xs n =
   equating (take n) xs (map f $ iterate (1+) 0)

equalFuncList2 :: (Integer -> Integer -> Integer) -> [[Integer]] -> Int -> Bool
equalFuncList2 f xs n =
   equating (take n) xs (zipWith (map . f) [0..] $ tail $ List.inits [0..])
