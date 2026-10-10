module Main (main) where

import Hedgehog (Property, property, success)
import Test.ExploreDocument (exploreDocumentTests)
import Test.ExtractHiCompare (extractHiCompareTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain . testGroup "aihc-dev" $
    [ testProperty "Hedgehog options" prop_dummy,
      exploreDocumentTests,
      extractHiCompareTests
      -- localPackageSubsetTests needs aihc-internal and aihc-template-haskell
      -- in a ghc-pkg database, which only the cabal build provides.
      -- fuzzTests
    ]

prop_dummy :: Property
prop_dummy = property success
