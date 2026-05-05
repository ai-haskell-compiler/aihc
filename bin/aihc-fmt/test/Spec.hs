module Main (main) where

import Test.Fmt.CLI (cliTests)
import Test.Fmt.Golden (goldenTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = do
  golden <- goldenTests
  defaultMain $
    testGroup
      "aihc-fmt"
      [ golden,
        cliTests,
        QC.testProperty "accepts shared QuickCheck options" prop_acceptsQuickCheckOptions
      ]

prop_acceptsQuickCheckOptions :: () -> Bool
prop_acceptsQuickCheckOptions () = True
