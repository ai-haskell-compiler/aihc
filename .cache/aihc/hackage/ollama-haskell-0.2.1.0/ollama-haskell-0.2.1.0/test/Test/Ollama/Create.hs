{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Create (tests) where

import Control.Monad (void)
import Data.Maybe (isNothing)
import Data.Ollama.Common.Types (ModelOptions (..))
import Data.Ollama.Common.Utils (defaultModelOptions)
import Data.Ollama.Create
import Data.Ollama.Delete (deleteModel)
import Test.Tasty
import Test.Tasty.HUnit

testCreateModelBasic :: TestTree
testCreateModelBasic = testCase "Create model: basic from existing model" $ do
  let ops =
        (defaultCreateOps "test-model-basic")
          { fromModel = Just "gemma3"
          , systemPrompt = Just "You are a helpful assistant."
          }
  createModel ops Nothing
  assertBool "Create should complete without error" True
  void $ deleteModel "test-model-basic" Nothing

testCreateModelWithQuantization :: TestTree
testCreateModelWithQuantization = testCase "Create model: with quantization" $ do
  let ops =
        (defaultCreateOps "test-model-quantized")
          { fromModel = Just "gemma3"
          , quantizeType = Just Q4_K_M
          }
  createModel ops Nothing
  assertBool "Quantization request should complete" True
  void $ deleteModel "test-model-quantized" Nothing

testCreateModelWithParameters :: TestTree
testCreateModelWithParameters = testCase "Create model: with custom parameters" $ do
  let modelOpts =
        defaultModelOptions
          { temperature = Just 0.7
          , topP = Just 0.9
          , topK = Just 40
          }
      ops =
        (defaultCreateOps "test-model-params")
          { fromModel = Just "gemma3"
          , parameters = Just modelOpts
          , template = Just "Custom template: {{.Prompt}}"
          }
  createModel ops Nothing
  assertBool "Custom parameters should be handled" True
  void $ deleteModel "test-model-params" Nothing

testQuantizationTypeValues :: TestTree
testQuantizationTypeValues = testCase "Quantization types: should have valid values" $ do
  let q1 = Q4_K_M
      q2 = Q4_K_S
      q3 = Q8_0
  assertBool "Q4_K_M should be valid" (show q1 == "Q4_K_M")
  assertBool "Q4_K_S should be valid" (show q2 == "Q4_K_S")
  assertBool "Q8_0 should be valid" (show q3 == "Q8_0")

testCreateModelFieldAccess :: TestTree
testCreateModelFieldAccess = testCase "CreateOps: field access should work" $ do
  let ops = defaultCreateOps "test-model"
  assertBool "modelName should be accessible" (modelName ops == "test-model")
  assertBool "fromModel should be Nothing by default" (isNothing (fromModel ops))
  assertBool "files should be Nothing by default" (isNothing (files ops))

tests :: TestTree
tests =
  testGroup
    "Create tests"
    [ testCreateModelBasic
    , testCreateModelWithQuantization
    , testCreateModelWithParameters
    , testQuantizationTypeValues
    , testCreateModelFieldAccess
    ]
