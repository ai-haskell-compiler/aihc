module Main where

import Test.Hspec
import Test.Data.WKT.ToWKT (toWktTests)
import Test.Data.WKT.FromWKT (fromWktTests)

main :: IO ()
main = hspec $ do
    toWktTests
    fromWktTests