module Main (main) where

import Test.CLI.Suite (cliTests)
import Test.Tasty

main :: IO ()
main = cliTests >>= defaultMain
