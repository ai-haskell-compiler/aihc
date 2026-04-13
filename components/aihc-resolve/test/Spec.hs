module Main (main) where

import Test.Resolver.Suite (resolverGoldenTests)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  resolverGolden <- resolverGoldenTests
  let dummyQC = QC.testProperty "dummy quickcheck property" prop_dummy
  defaultMain (testGroup "aihc-resolve" [resolverGolden, dummyQC])

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True
