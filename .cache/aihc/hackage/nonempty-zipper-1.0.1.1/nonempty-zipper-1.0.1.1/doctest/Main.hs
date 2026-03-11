module Main (main) where

import System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = do
  paths <- globDir1 (compile "**/*.hs") "library"
  doctest paths
