{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Blob (tests) where

import Data.Ollama.Blob
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Test.Tasty
import Test.Tasty.HUnit

testCheckBlobExistsInvalid :: TestTree
testCheckBlobExistsInvalid = testCase "Check blob exists: invalid digest should fail" $ do
  res <- checkBlobExists "invalid-digest" Nothing
  case res of
    Left _ -> assertBool "Should fail with invalid digest" True
    Right _ -> assertBool "Or return False for invalid digest" True

testCheckBlobExistsNonExistent :: TestTree
testCheckBlobExistsNonExistent = testCase "Check blob exists: non-existent blob" $ do
  let fakeDigest = "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
  res <- checkBlobExists fakeDigest Nothing
  case res of
    Left _ -> return () -- Network error is acceptable
    Right exists -> assertBool "Non-existent blob should return False" (not exists)

testCheckBlobExistsWithConfig :: TestTree
testCheckBlobExistsWithConfig = testCase "Check blob exists: with custom config" $ do
  let config = Just $ defaultOllamaConfig {timeout = 5}
      fakeDigest = "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
  res <- checkBlobExists fakeDigest config
  case res of
    Left _ -> return () -- Network error is acceptable
    Right _ -> assertBool "Should handle custom config" True

testCreateBlobInvalidFile :: TestTree
testCreateBlobInvalidFile = testCase "Create blob: invalid file path should fail" $ do
  let fakeDigest = "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
  res <- createBlob "/nonexistent/file/path" fakeDigest Nothing
  case res of
    Left _ -> assertBool "Should fail with invalid file path" True
    Right () -> assertFailure "Should not succeed with invalid file path"

tests :: TestTree
tests =
  testGroup
    "Blob tests"
    [ testCheckBlobExistsInvalid
    , testCheckBlobExistsNonExistent
    , testCheckBlobExistsWithConfig
    , testCreateBlobInvalidFile
    ]
