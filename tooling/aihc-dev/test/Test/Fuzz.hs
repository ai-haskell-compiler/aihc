module Test.Fuzz
  ( fuzzTests,
  )
where

import Aihc.Dev.Fuzz (batchSize, selectProperties)
import Aihc.Dev.Fuzz.CLI (Command (..), Selection (..), commandParser, parseDuration)
import Aihc.Dev.Fuzz.Registry (FuzzProperty (..), fuzzProperties, fuzzPropertyId)
import Aihc.Dev.Fuzz.TUI (Dashboard (..), renderDashboard, renderFrameUpdate)
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
          components,
      testCase "dashboard stays within short and narrow terminals" $ do
        let frame = renderDashboard 3 24 sampleDashboard
        assertEqual "uses available rows" 3 (length frame)
        assertBool "respects terminal width" (all ((<= 23) . length) frame)
        assertBool "reports hidden workers" (any ("more active" `isInfixOf`) frame),
      testCase "dashboard has a useful one-row fallback" $ do
        let frame = renderDashboard 1 18 sampleDashboard
        assertEqual "one row" 1 (length frame)
        assertBool "keeps identity" $ case frame of
          line : _ -> "AIHC FUZZ" `isInfixOf` line
          [] -> False,
      testCase "dashboard renders progress without filling unused rows" $ do
        let frame = renderDashboard 20 90 sampleDashboard
        assertBool "does not pad to terminal height" (length frame < 20)
        assertBool "shows active count" (any ("ACTIVE  5" `isInfixOf`) frame)
        assertBool "shows progress bar" (any ('━' `elem`) frame)
        assertBool "respects terminal width" (all ((<= 89) . length) frame),
      testCase "incremental renderer only updates changed rows" $ do
        let update = renderFrameUpdate 10 ["title", "old", "same"] ["title", "new", "same"]
        assertBool "does not clear screen" (not ("\ESC[2J" `isInfixOf` update))
        assertBool "leaves unchanged first row alone" (not ("\ESC[1;1H" `isInfixOf` update))
        assertBool "updates changed row" ("\ESC[2;1H" `isInfixOf` update),
      testCase "incremental renderer clears stale rows" $
        assertBool
          "clears removed second row"
          ("\ESC[2;1H\ESC[2K" `isInfixOf` renderFrameUpdate 10 ["title", "stale"] ["title"])
    ]

sampleDashboard :: Dashboard
sampleDashboard =
  Dashboard
    { dashboardActive =
        [ (1, "aihc-parser.expr round-trip", 1250),
          (2, "aihc-parser.decl round-trip", 5000),
          (3, "aihc-parser.module validator", 9999),
          (4, "aihc-tc.zonking idempotent", 2500),
          (5, "aihc-grin.pretty-printer round-trip", 7500)
        ],
      dashboardBatches = 42,
      dashboardCases = 424242,
      dashboardElapsed = 65,
      dashboardJobs = 5,
      dashboardProperties = 35
    }

isLeft :: Either left right -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False
