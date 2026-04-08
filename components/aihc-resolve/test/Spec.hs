module Main (main) where

import Test.Resolver.Suite (resolverGoldenTests)
import Test.Tasty

main :: IO ()
main = do
  resolverGolden <- resolverGoldenTests
  defaultMain (testGroup "aihc-resolve" [resolverGolden])
