{- |
Alternative implementations for functions in "Data.StorableVector.Lazy".
We want to use it for testing against the prefered implementations.
-}
module Alternative.Lazy where

import qualified Data.List.Match as Match
import qualified Data.List as List
import Data.Tuple.HT (mapFst)
import Data.Maybe.HT (toMaybe)


-- compact0 2 [] = [[]] - bad
-- compact0 2 [10] = [[],[10]] - bad
compact0 :: Int -> [Int] -> [[Int]]
compact0 maxS =
   let go _ xs [] = [xs]
       go s0 xs (y:ys) =
         let s1 = s0+y
         in  if s1<=maxS
               then go s1 (xs++[y]) ys
               else xs : go y [y] ys
   in  go 0 []

-- compact1 2 [] = [[]] - bad
-- compact1 2 [10] = [[],[10]] - bad
compact1 :: Int -> [Int] -> [[Int]]
compact1 maxS =
   let go _ [] = ([], [])
       go s0 (y:ys) =
         let s1 = s0+y
         in  if s1<=maxS
               then mapFst (y:) $ go s1 ys
               else ([], uncurry (:) $ mapFst (y:) $ go y ys)
   in  uncurry (:) . go 0

compact2 :: Int -> [Int] -> [[Int]]
compact2 maxS =
   let go _ [] = ([], [])
       go s0 (y:ys) =
         let s1 = s0+y
         in  if s1<=maxS
               then mapFst (y:) $ go s1 ys
               else ([], uncurry (:) $ mapFst (y:) $ go y ys)
   in  (\(xs,xss) -> if List.null xs then xss else xs:xss) . go 0

{- |
This is the counterpart to the actual implementation in StorableVector.Lazy.
-}
compact3 :: Int -> [Int] -> [[Int]]
compact3 maxS =
   (\(xs,xss) -> if List.null xs then xss else xs:xss) .
   (\xs ->
      List.foldr
         (\y go s0 ->
            let s1 = s0+y
            in  if s1<=maxS
                  then mapFst (y:) $ go s1
                  else ([], uncurry (:) $ mapFst (y:) $ go y))
         (const ([], [])) xs 0)

compact4 :: Int -> [Int] -> [[Int]]
compact4 maxS =
   List.unfoldr $ \xs -> toMaybe (not $ List.null xs) $
      Match.splitAt
         (minLength1 $ List.takeWhile (<= maxS) $
          List.tail $ List.scanl (+) 0 xs)
         xs

minLength1 :: [Int] -> [Int]
minLength1 = (0:) . List.drop 1
