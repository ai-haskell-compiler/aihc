module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest [
  "src/Data/Matrix/SymmetryOperationsSymbols.hs"
  -- ,"src/Data/Matrix/SymmetryOperationsSymbol/Common.hs"
  ]
