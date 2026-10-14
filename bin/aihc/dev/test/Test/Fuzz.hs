module Test.Fuzz
  ( fuzzTests,
  )
where

import Aihc.Dev.Fuzz.Bugs (Bug (..), Failure (..), IssueRef (..), bugFingerprint, commentBody, issueBody, parseIssueRef, parseReplay, recordFailure, replayCommand)
import Aihc.Dev.Fuzz.CLI (Command (..), Options (..), Selection (..), commandParser, defaultRepository, parseDuration)
import Aihc.Dev.Fuzz.Model (ReporterStatus (..), bugStatus, updateRates)
import Aihc.Dev.Fuzz.Registry (FuzzProperty (..), fuzzProperties, fuzzPropertyId)
import Aihc.Dev.Fuzz.Schedule (batchSize, cycleProperties, pickProperty, resolveJobCount, selectProperties)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.List (isInfixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Hedgehog.Internal.Property (Skip (..), skipCompress)
import Options.Applicative (ParserResult (CompletionInvoked, Success), defaultPrefs, execParserPure, info)
import Options.Applicative qualified as Options
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

fuzzTests :: TestTree
fuzzTests =
  testGroup
    "fuzz"
    [ testCase "uses 10,000-case scheduling batches" $
        assertEqual "batch size" 10000 batchSize,
      testCase "feeds one selected property to every worker" $
        assertEqual "assignments" (replicate 10 "only" :: [String]) (take 10 (cycleProperties [(1, "only")])),
      testCase "cycles property assignments without tracking active work" $
        assertEqual
          "assignments"
          (["one", "two", "three", "one", "two", "three", "one"] :: [String])
          (take 7 (cycleProperties [(1, "one"), (1, "two"), (1, "three")])),
      testCase "runs weight 1 ten times as often as weight 0.1" $ do
        let assignments = take 110 (cycleProperties [(1, "a" :: String), (0.1, "b")])
        assertEqual "weight 1" 100 (length (filter (== "a") assignments))
        assertEqual "weight 0.1" 10 (length (filter (== "b") assignments)),
      testCase "skips a resting property when another one is ready" $ do
        let schedule = cycle ["a", "b", "c" :: String]
            picked resting = fmap (\(item, wait, rest) -> (item, wait, take 4 rest)) (pickProperty (`elem` resting) 3 schedule)
        assertEqual "ready head" (Just ("a", False, ["b", "c", "a", "b"])) (picked [])
        assertEqual "resting head" (Just ("b", False, ["a", "c", "a", "b"])) (picked ["a"])
        assertEqual "all resting" (Just ("a", True, ["b", "c", "a", "b"])) (picked ["a", "b", "c"])
        assertEqual "empty" Nothing (picked [] >> pickProperty (const False) 3 ([] :: [String])),
      testCase "does not cap jobs at the selected property count" $
        assertEqual "jobs" 10 (resolveJobCount 4 (Just 10)),
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
          Options.Failure failure -> fail (show failure)
          CompletionInvoked _ -> fail "unexpected shell completion",
      testCase "parses the report options of a run" $
        case execParserPure defaultPrefs (info commandParser mempty) ["--no-report", "--bug-file", "bugs.json"] of
          Success (Run options) -> do
            assertEqual "repository" defaultRepository (optionsRepository options)
            assertBool "reporting is off" (not (optionsReport options))
            assertEqual "bug file" (Just "bugs.json") (optionsBugFile options)
          Success other -> fail ("unexpected command " <> show other)
          Options.Failure failure -> fail (show failure)
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
          (Set.fromList ["aihc-grin", "aihc-tc", "aihc-gc"])
          components,
      testCase "fingerprints ignore the seed and keep the counterexample" $ do
        let otherSeed = sampleFailure {failureSeed = "Seed 9 9", failureReplay = "1:a Seed 9 9", failureReport = "other report"}
            otherValue = sampleFailure {failureValues = ["[2]"]}
        assertEqual "same bug" (bugFingerprint sampleFailure) (bugFingerprint otherSeed)
        assertBool "different bug" (bugFingerprint sampleFailure /= bugFingerprint otherValue)
        assertEqual "sixteen hex digits" 16 (length (bugFingerprint sampleFailure)),
      testCase "counts a repeated failure as a hit of the known bug" $ do
        let (first, firstNew, afterFirst) = recordFailure (time 1) sampleFailure []
            (second, secondNew, afterSecond) = recordFailure (time 2) sampleFailure {failureSeed = "Seed 9 9"} afterFirst
            (third, thirdNew, afterThird) = recordFailure (time 3) sampleFailure {failureValues = ["[2]"]} afterSecond
        assertBool "first is new" firstNew
        assertEqual "first hit" 1 (bugHits first)
        assertBool "second is known" (not secondNew)
        assertEqual "second hit" 2 (bugHits second)
        assertEqual "first seen is kept" (time 1) (bugFirstSeen second)
        assertEqual "last seen moves" (time 2) (bugLastSeen second)
        assertEqual "one bug" 1 (length afterSecond)
        assertBool "third is new" thirdNew
        assertEqual "two bugs" 2 (length afterThird)
        assertEqual "the first failure is kept" sampleFailure (bugFailure third {bugFailure = sampleFailure}),
      testCase "bug store round-trips through JSON" $ do
        let (_, _, bugs) = recordFailure (time 1) sampleFailure []
            reported = map (\bug -> bug {bugIssue = Just (IssueRef 2690 "https://github.com/o/r/issues/2690")}) bugs
        assertEqual "pending" (Right bugs) (Aeson.eitherDecode (Aeson.encode bugs))
        assertEqual "reported" (Right reported) (Aeson.eitherDecode (Aeson.encode reported)),
      testCase "reads the issue reference from gh output" $ do
        assertEqual
          "url"
          (Just (IssueRef 2690 "https://github.com/o/r/issues/2690"))
          (parseIssueRef "Creating issue in o/r\n\nhttps://github.com/o/r/issues/2690\n")
        assertEqual "no url" Nothing (parseIssueRef "error: not found"),
      testCase "issue text carries the replay command and the report" $ do
        let (bug, _, _) = recordFailure (time 1) sampleFailure []
        assertBool "title in body" (replayCommand sampleFailure `isInfixOf` issueBody bug)
        assertBool "report in body" (failureReport sampleFailure `isInfixOf` issueBody bug)
        assertBool "replay in comment" (replayCommand sampleFailure `isInfixOf` commentBody bug)
        assertEqual "replay command" "cabal run -v0 aihc-dev -- fuzz replay \"aihc-tc.zonking idempotent\" \"3:b2 Seed 1 3\"" (replayCommand sampleFailure),
      testCase "parses a replay argument with and without a shrink path" $ do
        let compressed = fmap (bimap skipCompress show)
        assertEqual "shrink path" (Right ("3:b2", "Seed 1 3")) (compressed (parseReplay "3:b2 Seed 1 3"))
        assertEqual "seed only" (Right (skipCompress SkipNothing, "Seed 1 3")) (compressed (parseReplay "Seed 1 3"))
        assertBool "garbage" (isLeft (parseReplay "nonsense")),
      testCase "describes pending bugs with the reporter state" $ do
        let (bug, _, _) = recordFailure (time 1) sampleFailure []
        assertEqual "idle" "pending" (bugStatus ReporterIdle bug)
        assertEqual "blocked" "pending · gh is not installed" (bugStatus (ReporterBlocked "gh is not installed") bug)
        assertEqual "sending" "reporting…" (bugStatus (ReporterSending (bugId bug)) bug)
        assertEqual "sending another" "pending" (bugStatus (ReporterSending "other") bug)
        assertEqual "reported" "#7" (bugStatus ReporterIdle bug {bugIssue = Just (IssueRef 7 "url")}),
      testCase "blends case rates from the sample difference" $ do
        let previous = Map.fromList [("a", 100)]
            current = Map.fromList [("a", 300), ("b", 50)]
            rates = updateRates 0.5 previous current (Map.fromList [("a", 1000)])
        assertEqual "smoothed rate" (Just 880) (Map.lookup "a" rates)
        assertEqual "new property starts from zero" (Just 100) (Map.lookup "b" rates)
        assertEqual "no time, no change" (Map.fromList [("a", 1000)]) (updateRates 0 previous current (Map.fromList [("a", 1000)]))
    ]

sampleFailure :: Failure
sampleFailure =
  Failure
    { failureProperty = "aihc-tc.zonking idempotent",
      failureSeed = "Seed 1 3",
      failureReplay = "3:b2 Seed 1 3",
      failureText = "zonk changed the type",
      failureValues = ["[1]"],
      failureSite = Just "Fuzz.hs:12",
      failureReport = "✗ failed after 3 tests.\n  zonk changed the type"
    }

time :: Integer -> UTCTime
time seconds = UTCTime (fromGregorian 2026 10 14) (fromIntegral seconds)

isLeft :: Either left right -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False
