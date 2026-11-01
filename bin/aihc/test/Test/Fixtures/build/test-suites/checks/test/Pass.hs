module Main (main) where

import Counter (count)
import System.Environment (getArgs)

-- The data file is read from the working directory, which is the
-- directory of the package.
main :: IO ()
main = do
  arguments <- getArgs
  expected <- readFile "data/expected.txt"
  putStrLn ("count " ++ show (count "abc") ++ " expected " ++ takeWhile (/= '\n') expected)
  mapM_ putStrLn arguments
