module Main (main) where

square :: Int -> Int
square x = x * x

main :: IO ()
main = print (sum (map square [1 .. 10]))
