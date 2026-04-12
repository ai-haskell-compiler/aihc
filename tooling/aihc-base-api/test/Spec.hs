module Main (main) where

import Test.Tasty (defaultMain)
import Test.Extractor.Suite (tests)

main :: IO ()
main = defaultMain tests
