{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Load (tests) where

import Data.Ollama.Load
import Test.Tasty
import Test.Tasty.HUnit

testLoadGenModelBasic :: TestTree
testLoadGenModelBasic = testCase "Load generation model: basic functionality" $ do
  res <- loadGenModel "gemma3"
  case res of
    Left _ -> assertFailure "Expected success, got error"
    Right () -> assertBool "Load should succeed" True

testUnloadGenModelBasic :: TestTree
testUnloadGenModelBasic = testCase "Unload generation model: basic functionality" $ do
  res <- unloadGenModel "gemma3"
  case res of
    Left _ -> assertFailure "Expected success, got error"
    Right () -> assertBool "Unload should succeed" True

testLoadInvalidModel :: TestTree
testLoadInvalidModel = testCase "Load invalid model should fail" $ do
  res <- loadGenModel "nonexistent-model-12345"
  case res of
    Left _ -> assertBool "Should fail with invalid model" True
    Right () -> assertFailure "Should not succeed with invalid model"

testUnloadInvalidModel :: TestTree
testUnloadInvalidModel = testCase "Unload invalid model should handle gracefully" $ do
  res <- unloadGenModel "nonexistent-model-12345"
  case res of
    Left _ -> assertBool "Should fail or handle gracefully" True
    Right () -> assertBool "Or succeed if server handles gracefully" True

testLoadEmptyModelName :: TestTree
testLoadEmptyModelName = testCase "Load model: empty name should fail" $ do
  res <- loadGenModel ""
  case res of
    Left _ -> assertBool "Should fail with empty name" True
    Right () -> assertFailure "Should not succeed with empty name"

tests :: TestTree
tests =
  testGroup
    "Load tests"
    [ testLoadGenModelBasic
    , testUnloadGenModelBasic
    , testLoadInvalidModel
    , testUnloadInvalidModel
    , testLoadEmptyModelName
    ]
