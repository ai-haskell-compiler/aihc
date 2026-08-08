module Main where

import System.Exit (ExitCode (ExitFailure), exitWith)

main :: IO ()
main = exitWith (ExitFailure 7)
