module Main (main) where

import Test.CLI.Suite (cliTests)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  cliTestsTree <- cliTests
  let dummyQC = QC.testProperty "dummy quickcheck property" prop_dummy
  defaultMain (testGroup "aihc-parser-cli" [cliTestsTree, dummyQC])

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True
