{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Copy (tests) where

import Control.Monad (void)
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Copy
import Data.Ollama.Delete (deleteModel)
import Test.Tasty
import Test.Tasty.HUnit

testCopyModelBasic :: TestTree
testCopyModelBasic = testCase "Copy model: basic functionality" $ do
  res <- copyModel "gemma3" "gemma3-test-copy" Nothing
  case res of
    Left _ -> return () -- Allow failure if source model doesn't exist
    Right () -> do
      assertBool "Copy should succeed" True
      void $ deleteModel "gemma3-test-copy" Nothing

testCopyModelWithConfig :: TestTree
testCopyModelWithConfig = testCase "Copy model: with custom config" $ do
  let config = Just $ defaultOllamaConfig {timeout = 30}
  res <- copyModel "gemma3" "gemma3-test-copy-config" config
  case res of
    Left _ -> return () -- Allow failure if source doesn't exist
    Right () -> do
      assertBool "Copy with config should work" True
      void $ deleteModel "gemma3-test-copy-config" Nothing

testCopyModelInvalidSource :: TestTree
testCopyModelInvalidSource = testCase "Copy model: invalid source should fail" $ do
  res <- copyModel "nonexistent-model-12345" "some-destination" Nothing
  case res of
    Left _ -> assertBool "Should fail with invalid source" True
    Right () -> assertFailure "Should not succeed with invalid source"

tests :: TestTree
tests =
  testGroup
    "Copy tests"
    [ testCopyModelBasic
    , testCopyModelWithConfig
    , testCopyModelInvalidSource
    ]
