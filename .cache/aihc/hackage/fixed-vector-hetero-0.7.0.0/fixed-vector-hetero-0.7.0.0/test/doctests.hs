module Main where

import Data.Foldable  (traverse_)
import Test.DocTest   (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args
    doctest args
  where
    args = ["Data"]

