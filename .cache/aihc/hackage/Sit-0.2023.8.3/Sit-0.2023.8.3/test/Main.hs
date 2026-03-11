-- | Test runner for Sit.

module Main where

import Sit       (checkFile)
import Paths_Sit (getDataFileName)

main :: IO ()
main = checkFile =<< getDataFileName "test/Test.agda"
