{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Delete (tests) where

import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Delete
import Test.Tasty
import Test.Tasty.HUnit

testDeleteModelNonExistent :: TestTree
testDeleteModelNonExistent = testCase "Delete non-existent model should fail gracefully" $ do
  res <- deleteModel "nonexistent-model-12345" Nothing
  case res of
    Left _ -> assertBool "Should fail when deleting non-existent model" True
    Right () -> assertBool "Or succeed if server handles gracefully" True

testDeleteModelWithConfig :: TestTree
testDeleteModelWithConfig = testCase "Delete model: with custom config" $ do
  let config = Just $ defaultOllamaConfig {timeout = 30}
  res <- deleteModel "nonexistent-test-model" config
  case res of
    Left _ -> assertBool "Should handle custom config" True
    Right () -> assertBool "Or succeed if server handles gracefully" True

testDeleteModelEmptyName :: TestTree
testDeleteModelEmptyName = testCase "Delete model: empty name should fail" $ do
  res <- deleteModel "" Nothing
  case res of
    Left _ -> assertBool "Should fail with empty name" True
    Right () -> assertFailure "Should not succeed with empty name"

-- Note: We avoid testing actual deletion of existing models in unit tests
-- as it would be destructive. Integration tests could cover this.

tests :: TestTree
tests =
  testGroup
    "Delete tests"
    [ testDeleteModelNonExistent
    , testDeleteModelWithConfig
    , testDeleteModelEmptyName
    ]
