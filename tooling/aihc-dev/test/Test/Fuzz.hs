module Test.Fuzz
  ( fuzzTests,
  )
where

import Aihc.Dev.Fuzz (batchSize, selectProperties)
import Aihc.Dev.Fuzz.CLI (Command (..), Selection (..), commandParser, parseDuration)
import Aihc.Dev.Fuzz.Registry (FuzzProperty (..), fuzzProperties, fuzzPropertyId)
import Data.List (isInfixOf, nub)
import Data.Set qualified as Set
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, info)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

fuzzTests :: TestTree
fuzzTests =
  testGroup
    "fuzz"
    [ testCase "uses 10,000-case scheduling batches" $
        assertEqual "batch size" 10000 batchSize,
      testCase "parses supported time-limit units" $ do
        assertEqual "milliseconds" (Right 0.25) (parseDuration "250ms")
        assertEqual "seconds" (Right 30) (parseDuration "30s")
        assertEqual "fractional minutes" (Right 90) (parseDuration "1.5m")
        assertEqual "hours" (Right 7200) (parseDuration "2h"),
      testCase "rejects invalid time limits" $ do
        assertBool "missing unit" (isLeft (parseDuration "30"))
        assertBool "zero" (isLeft (parseDuration "0s"))
        assertBool "unknown unit" (isLeft (parseDuration "1d")),
      testCase "parses the list subcommand with selection flags" $
        case execParserPure defaultPrefs (info commandParser mempty) ["list", "--filter", "TC", "--exclude", "reflexive"] of
          Success command ->
            assertEqual
              "command"
              (List (Selection ["TC"] ["reflexive"]))
              command
          Failure failure -> fail (show failure)
          CompletionInvoked _ -> fail "unexpected shell completion",
      testCase "filters and excludes component-qualified names case-insensitively" $ do
        let selected = selectProperties (Selection ["ROUND-TRIP"] ["compat"]) fuzzProperties
            identifiers = map fuzzPropertyId selected
        assertBool "selected at least one property" (not (null selected))
        assertBool "all match filter" (all ("round-trip" `isInfixOf`) identifiers)
        assertBool "none match exclusion" (not (any ("compat" `isInfixOf`) identifiers)),
      testCase "registry has unique properties organized by component" $ do
        let identifiers = map fuzzPropertyId fuzzProperties
            components = Set.fromList (map fuzzPropertyComponent fuzzProperties)
        assertEqual "unique identifiers" (length identifiers) (length (nub identifiers))
        assertEqual
          "components"
          (Set.fromList ["aihc-grin", "aihc-parser", "aihc-parser-compat", "aihc-tc"])
          components
    ]

isLeft :: Either left right -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False
