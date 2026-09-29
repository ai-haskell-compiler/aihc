{-# LANGUAGE BangPatterns #-}

module Main (main) where

sumTo :: Int -> Int
sumTo n = go 0 1
  where
    go :: Int -> Int -> Int
    go !acc i
      | i > n = acc
      | otherwise = go (acc + i) (i + 1)

main :: IO ()
main = print (sumTo 100)
