{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ParserQuickCheck.Runner
  ( runnerTests,
  )
where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import ParserQuickCheck.Runner
import ParserQuickCheck.Types
import Test.QuickCheck (Property, chooseInt, counterexample, forAll, once, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

runnerTests :: TestTree
runnerTests =
  testGroup
    "runner"
    [ testCase "applies the same max-success to all selected properties" $ do
        batchResult <- runBatch fixedTime sampleConfig [alwaysPassProperty "alpha", alwaysPassProperty "beta"]
        assertEqual "expected selected properties to stay ordered" ["alpha", "beta"] (map propertyName (results batchResult))
        assertEqual "expected max-success on each property" [7, 7] (map actualTests (results batchResult)),
      testCase "filters to one property" $
        case selectProperties (Just "beta") [alwaysPassProperty "alpha", alwaysPassProperty "beta"] of
          Right selected ->
            assertEqual "expected exactly one property" ["beta"] (map registeredPropertyName selected)
          Left err -> assertFailure err,
      testCase "reruns deterministically for a fixed seed" $ do
        first <- runBatch fixedTime sampleConfig [seedSensitiveFailureProperty]
        second <- runBatch fixedTime sampleConfig [seedSensitiveFailureProperty]
        assertEqual "expected stable derived seed" (map seed (results first)) (map seed (results second))
        assertEqual "expected stable failure transcript" (map failureTranscript (results first)) (map failureTranscript (results second)),
      testCase "normalizes fingerprints consistently" $
        let leftFingerprint = failureFingerprint "prop" "line one  \r\nline two\r\n"
            rightFingerprint = failureFingerprint "prop" "line one\nline two\n"
         in assertEqual "expected equivalent fingerprints" leftFingerprint rightFingerprint,
      testCase "encodes and decodes JSON results" $ do
        let encoded = Aeson.encode sampleBatchResult
            decoded = Aeson.decode encoded
        assertEqual "expected round-trippable JSON" (Just sampleBatchResult) decoded,
      testCase "marks failures as requiring attention" $
        assertBool "expected FAIL to require attention" (statusRequiresAttention FAIL),
      testCase "pass results do not require attention" $
        assertBool "expected PASS to not require attention" (not (statusRequiresAttention PASS))
    ]

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 3 20) 0

sampleConfig :: BatchConfig
sampleConfig =
  BatchConfig
    { batchCommitSha = "deadbeef",
      batchMaxSuccess = 7,
      batchSeedValue = 12345
    }

alwaysPassProperty :: Text -> RegisteredProperty
alwaysPassProperty name = RegisteredProperty name (property True)

seedSensitiveFailureProperty :: RegisteredProperty
seedSensitiveFailureProperty =
  RegisteredProperty
    "seed-sensitive-failure"
    seedSensitiveFailure

seedSensitiveFailure :: Property
seedSensitiveFailure =
  once $
    forAll (chooseInt (0, 1000)) $ \value ->
      counterexample ("value=" <> show value) False

sampleBatchResult :: BatchResult
sampleBatchResult =
  BatchResult
    { generatedAt = "2026-03-20T00:00:00Z",
      batchSeed = 12345,
      commitSha = "deadbeef",
      configuredMaxSuccess = 10000,
      selectedProperties = ["alpha", "beta"],
      results =
        [ PropertyResult
            { propertyName = "alpha",
              status = PASS,
              seed = 11,
              configuredMaxSuccess = 10000,
              actualTests = 10000,
              actualDiscarded = 0,
              failureTranscript = Nothing,
              fingerprint = Nothing,
              reason = Nothing,
              reproductionCommand = "nix run .#parser-quickcheck-batch -- --max-success 10000 --seed 12345 --property 'alpha'"
            },
          PropertyResult
            { propertyName = "beta",
              status = FAIL,
              seed = 22,
              configuredMaxSuccess = 10000,
              actualTests = 1,
              actualDiscarded = 0,
              failureTranscript = Just "boom",
              fingerprint = Just "abc123",
              reason = Just "Falsified",
              reproductionCommand = "nix run .#parser-quickcheck-batch -- --max-success 10000 --seed 12345 --property 'beta'"
            }
        ]
    }
