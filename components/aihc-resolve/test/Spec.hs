module Main (main) where

import Hedgehog (Property, property, success)
import Test.Resolver.Suite (resolverGoldenTests, resolverUnitTests)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
  resolverGolden <- resolverGoldenTests
  let hedgehogOptions = testProperty "Hedgehog options" prop_dummy
  defaultMain (testGroup "aihc-resolve" [resolverGolden, resolverUnitTests, hedgehogOptions])

-- | Keep the repository Hedgehog options accepted by this test suite.
prop_dummy :: Property
prop_dummy = property success
