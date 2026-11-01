module Main (main) where

import System.Exit (ExitCode (..), exitWith)

main :: IO ()
main = do
  putStrLn "fail runs"
  exitWith (ExitFailure 3)
