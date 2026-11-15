module Main (main) where

import Triangle (triangle, twice)

main :: IO ()
main = do
  print (triangle 10)
  print (twice 21)
