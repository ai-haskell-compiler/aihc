module Test.ParserQuickCheck.CLI
  ( cliTests,
  )
where

import ParserQuickCheck.CLI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

cliTests :: TestTree
cliTests =
  testGroup
    "cli"
    [ testCase "parses explicit options" $
        case parseOptionsPure ["--max-success", "42", "--seed", "7", "--property", "demo", "--json"] of
          Right options ->
            assertEqual
              "expected option parse result"
              (Options {optMaxSuccess = 42, optSeed = Just 7, optProperty = Just "demo", optJson = True})
              options
          Left err -> assertFailure err,
      testCase "uses default max-success" $
        case parseOptionsPure [] of
          Right options -> assertEqual "expected default max-success" 10000 (optMaxSuccess options)
          Left err -> assertFailure err
    ]
