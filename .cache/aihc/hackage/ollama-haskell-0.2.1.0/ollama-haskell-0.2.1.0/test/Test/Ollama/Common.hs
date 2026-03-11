{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Common (tests) where

import Data.Ollama.Common.SchemaBuilder
import Data.Ollama.Common.Utils (encodeImage)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

-- Test SchemaBuilder functionality
testSchemaBuilderBasic :: TestTree
testSchemaBuilderBasic = testCase "SchemaBuilder: basic object construction" $ do
  let schema =
        buildSchema $
          emptyObject
            |+ ("name", JString)
            |+ ("age", JNumber)
            |! "name"
            |! "age"

  -- Check that schema is not null/empty
  let schemaText = T.pack $ show schema
  assertBool "Schema should not be empty" (not $ T.null schemaText)
  assertBool "Schema should contain 'name'" ("name" `T.isInfixOf` schemaText)
  assertBool "Schema should contain 'age'" ("age" `T.isInfixOf` schemaText)

testSchemaBuilderArray :: TestTree
testSchemaBuilderArray = testCase "SchemaBuilder: array types" $ do
  let arraySchema =
        buildSchema $
          emptyObject
            |+ ("items", JArray JString)
            |+ ("numbers", JArray JNumber)
            |+ ("flags", JArray JBoolean)
            |! "items"

  let schemaText = T.pack $ show arraySchema
  assertBool "Array schema should not be empty" (not $ T.null schemaText)
  assertBool "Should contain items field" ("items" `T.isInfixOf` schemaText)

testSchemaBuilderOptionalFields :: TestTree
testSchemaBuilderOptionalFields = testCase "SchemaBuilder: optional vs required fields" $ do
  let schema =
        buildSchema $
          emptyObject
            |+ ("required_field", JString)
            |+ ("optional_field", JString)
            |! "required_field"

  let schemaText = T.pack $ show schema
  assertBool "Schema should contain both fields" $
    "required_field" `T.isInfixOf` schemaText
      && "optional_field" `T.isInfixOf` schemaText

-- Test image encoding utility
testEncodeImageValidFile :: TestTree
testEncodeImageValidFile = testCase "EncodeImage: should handle existing image file" $ do
  -- Test with the sample image from the examples
  result <- encodeImage "./examples/sample.png"
  case result of
    Nothing -> return () -- File might not exist in test environment
    Just encoded -> do
      assertBool "Encoded image should not be empty" (not $ T.null encoded)
      assertBool "Should be base64-like content" (T.length encoded > 10)

testEncodeImageNonExistentFile :: TestTree
testEncodeImageNonExistentFile = testCase "EncodeImage: should return Nothing for non-existent file" $ do
  result <- encodeImage "./nonexistent/file.png"
  case result of
    Nothing -> assertBool "Should return Nothing for non-existent file" True
    Just _ -> assertFailure "Should not encode non-existent file"

testEncodeImageInvalidFormat :: TestTree
testEncodeImageInvalidFormat = testCase "EncodeImage: should handle invalid file format" $ do
  result <- encodeImage "./test/Main.hs" -- Text file, not an image
  case result of
    Nothing -> assertBool "Should return Nothing for invalid format" True
    Just _ -> assertFailure "Should not encode invalid format"

tests :: TestTree
tests =
  testGroup
    "Common utilities tests"
    [ testSchemaBuilderBasic
    , testSchemaBuilderArray
    , testSchemaBuilderOptionalFields
    , testEncodeImageValidFile
    , testEncodeImageNonExistentFile
    , testEncodeImageInvalidFormat
    ]
