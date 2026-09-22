module Main where

-- Pipelines that the list fusion rules of the core libraries rewrite: each
-- line prints the same value at every optimization level.

main :: IO ()
main = do
  print (sum (map (* 2) (filter even [1 .. 100 :: Int])))
  print (length (filter odd (map (+ 1) [1 .. 1000 :: Int])))
  print (map (+ 1) (map (* 3) [1 .. 5 :: Int]))
  print (filter (> 3) (filter (< 8) [1 .. 10 :: Int]))
  print (foldr (\x acc -> x + acc) 0 (map (* 2) [1 .. 10 :: Int]))
  print (and (map (> 0) [1 .. 20 :: Int]))
  print (any (== 7) (map (* 7) [1 .. 3 :: Int]))
  print (concatMap (\x -> [x, x]) [1 .. 3 :: Int])
  print (map show [1 .. 3 :: Int] ++ map show [4 .. 6 :: Int])
  print (takeWhile (< 5) (map (+ 1) [0 .. 10 :: Int]))
  print (sum (map (* 2) [] :: [Int]))
  print (length (filter even [5 .. 4 :: Int]))
