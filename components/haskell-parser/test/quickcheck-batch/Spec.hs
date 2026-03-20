module Main (main) where

import Test.ParserQuickCheck.CLI (cliTests)
import Test.ParserQuickCheck.Runner (runnerTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "parser-quickcheck-tests"
      [ cliTests,
        runnerTests
      ]
