{-# LANGUAGE OverloadedStrings #-}

import Aihc.Parser.Syntax (Extension, parseExtensionName)
import Aihc.Testing.FixtureIndex
  ( FixtureEntry (..),
    FixtureStatus (..),
    FixtureSuite (..),
    parseFixtureEntry,
  )
import Data.ByteString.Char8 qualified as BS8
import Data.Either (isLeft)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Hedgehog (property, success)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain $
    testGroup
      "aihc-testing"
      [ testCase "reads fixture extensions and status" testFixtureEntry,
        testCase "defaults fixture extensions to none" testFixtureEntryNoExtensions,
        testCase "rejects an unknown fixture extension" testFixtureUnknownExtension,
        testCase "rejects an unknown fixture status" testFixtureUnknownStatus,
        testProperty "accepts repository Hedgehog options" (property success)
      ]

testFixtureEntry :: IO ()
testFixtureEntry = do
  entry <- decodeFixtureOrFail EvalSuite "extensions:\n  - MagicHash\n  - UnboxedTuples\nstatus: XFail\n"
  assertEqual "suite" EvalSuite (entrySuite entry)
  assertEqual "path" "demo.yaml" (entryPath entry)
  assertEqual
    "extensions"
    (map parseExtensionOrFail ["MagicHash", "UnboxedTuples"])
    (entryExtensions entry)
  assertEqual "status" StatusXFail (entryStatus entry)

testFixtureEntryNoExtensions :: IO ()
testFixtureEntryNoExtensions = do
  entry <- decodeFixtureOrFail FcGoldenSuite "status: fail\n"
  assertEqual "suite" FcGoldenSuite (entrySuite entry)
  assertEqual "extensions" [] (entryExtensions entry)
  assertEqual "status" StatusFail (entryStatus entry)

testFixtureUnknownExtension :: IO ()
testFixtureUnknownExtension =
  assertBool
    "expected unknown extension rejection"
    (isLeft (decodeFixture EvalSuite "extensions:\n  - NoSuchExtension\nstatus: pass\n"))

testFixtureUnknownStatus :: IO ()
testFixtureUnknownStatus =
  assertBool
    "expected unknown status rejection"
    (isLeft (decodeFixture EvalSuite "status: maybe\n"))

decodeFixture :: FixtureSuite -> String -> Either String FixtureEntry
decodeFixture suite source =
  case Yaml.decodeEither' (BS8.pack source) of
    Left err -> Left (show err)
    Right value -> parseFixtureEntry suite "demo.yaml" value

decodeFixtureOrFail :: FixtureSuite -> String -> IO FixtureEntry
decodeFixtureOrFail suite source =
  case decodeFixture suite source of
    Left err -> assertFailure err
    Right entry -> pure entry

parseExtensionOrFail :: Text.Text -> Extension
parseExtensionOrFail name =
  case parseExtensionName name of
    Just ext -> ext
    Nothing -> error ("parseExtensionOrFail: " <> Text.unpack name)
