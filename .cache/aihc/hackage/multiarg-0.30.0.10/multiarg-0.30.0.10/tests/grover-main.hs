module Main where

import Multiarg.Examples.Grover
import System.Environment

main :: IO ()
main = do
  as <- getArgs
  putStrLn . show $ parseGrover as
