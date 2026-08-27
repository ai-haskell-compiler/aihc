module Main (main) where

import Test.Tasty (defaultMain)
import Test.Wasm.Suite (tests)

main :: IO ()
main = defaultMain tests
