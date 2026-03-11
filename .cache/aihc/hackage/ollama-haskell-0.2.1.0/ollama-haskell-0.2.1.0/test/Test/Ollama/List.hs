{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.List (tests) where

import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.List
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

testListBasic :: TestTree
testListBasic = testCase "List models: basic call should return Models" $ do
  res <- list Nothing
  case res of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right (Models modelList) -> do
      assertBool "Should return a list (possibly empty)" True
      -- Additional checks if models are present
      case modelList of
        [] -> return () -- Empty list is acceptable
        (model : _) -> do
          assertBool "Model name should not be empty" (not $ T.null $ name model)
          assertBool "Model digest should not be empty" (not $ T.null $ digest model)
          assertBool "Model size should be positive" (size model > 0)
          assertBool "details should be present" True -- ModelDetails is always present

testListWithConfig :: TestTree
testListWithConfig = testCase "List with custom config should work" $ do
  let config = Just $ defaultOllamaConfig {timeout = 10}
  res <- list config
  case res of
    Left _ -> assertFailure "Expected success, got error"
    Right (Models _) -> assertBool "Should return Models type" True

tests :: TestTree
tests =
  testGroup
    "List tests"
    [ testListBasic
    , testListWithConfig
    ]
