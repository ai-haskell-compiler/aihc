module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest [
  "src/Data/Ratio/Slash.hs",
  "src/Data/Ratio/ParseFloat.hs",
  "src/Data/Matrix/AsXYZ/ParseXYZ.hs",
  "src/Data/Matrix/AsXYZ/ParseXY.hs",
  "src/Data/Matrix/AsXYZ.hs"
  ]
