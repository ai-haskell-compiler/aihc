{-# LANGUAGE OverloadedStrings #-}

module Test.Performance.Suite
  ( parserPerformanceTests,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax (Extension, parseExtensionName)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (toLower)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import ParserGolden (ExpectedStatus (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

data PerfCase = PerfCase
  { perfCaseId :: !String,
    perfCaseSourceName :: !FilePath,
    perfCaseExtensions :: ![Extension],
    perfCaseInput :: !Text,
    perfCaseStatus :: !ExpectedStatus,
    perfCaseReason :: !String
  }

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/performance/module"

timeoutMicros :: Int
timeoutMicros = 1000000

parserPerformanceTests :: IO TestTree
parserPerformanceTests = do
  cases <- loadPerfCases
  pure $
    testGroup
      "performance"
      [ testGroup "module-parse-under-1s" (map mkPerfCaseTest cases)
      ]

mkPerfCaseTest :: PerfCase -> TestTree
mkPerfCaseTest perfCase =
  testCase (perfCaseId perfCase) (assertPerfCase perfCase)

assertPerfCase :: PerfCase -> Assertion
assertPerfCase perfCase = do
  outcome <-
    timeout timeoutMicros $
      evaluate $
        force $
          parseModule
            defaultConfig
              { parserSourceName = perfCaseSourceName perfCase,
                parserExtensions = perfCaseExtensions perfCase
              }
            (perfCaseInput perfCase)
  case (perfCaseStatus perfCase, outcome) of
    (StatusPass, Nothing) ->
      assertFailure
        ( "module parse exceeded "
            <> show timeoutMicros
            <> "us for "
            <> perfCaseId perfCase
        )
    (StatusPass, Just (errs, _))
      | not (null errs) ->
          assertFailure
            ( "expected parse success for performance case "
                <> perfCaseId perfCase
                <> ", got parse error: "
                <> formatParseErrors (perfCaseSourceName perfCase) (Just (perfCaseInput perfCase)) errs
            )
    (StatusXFail, Just (errs, _))
      | null errs ->
          assertFailure
            ( "Unexpected pass in xfail performance case "
                <> perfCaseId perfCase
                <> " reason="
                <> perfCaseReason perfCase
            )
    (StatusXFail, _) -> pure ()
    _ -> pure ()

loadPerfCases :: IO [PerfCase]
loadPerfCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadPerfCase paths

loadPerfCase :: FilePath -> IO PerfCase
loadPerfCase path = do
  source <- TIO.readFile path
  case parsePerfCaseText path source of
    Left err -> fail err
    Right perfCase -> pure perfCase

parsePerfCaseText :: FilePath -> Text -> Either String PerfCase
parsePerfCaseText path source = do
  value <-
    case Y.decodeEither' (TE.encodeUtf8 source) of
      Left err -> Left ("Invalid performance fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputText, statusText, reasonText) <-
    case parseEither
      ( withObject "performance fixture" $ \obj -> do
          exts <- obj .:? "extensions" .!= []
          inputText <- obj .: "input"
          status <- obj .:? "status" .!= "pass"
          reason <- obj .:? "reason" .!= ""
          pure (exts, inputText, status, reason)
      )
      value of
      Left err -> Left ("Invalid performance fixture schema in " <> path <> ": " <> err)
      Right parsed -> Right parsed
  exts <- traverse (parseExtension path) extNames
  status <- parseStatus path statusText
  pure
    PerfCase
      { perfCaseId = dropRootPrefix path,
        perfCaseSourceName = dropRootPrefix path,
        perfCaseExtensions = exts,
        perfCaseInput = inputText,
        perfCaseStatus = status,
        perfCaseReason = T.unpack reasonText
      }
  where
    parseExtension fixturePath raw =
      case parseExtensionName raw of
        Just ext -> Right ext
        Nothing -> Left ("Unknown parser extension " <> show raw <> " in " <> fixturePath)
    parseStatus fixturePath raw =
      case map toLower (T.unpack (T.strip raw)) of
        "pass" -> Right StatusPass
        "xfail" -> Right StatusXFail
        _ -> Left ("Invalid [status] in " <> fixturePath <> ": " <> T.unpack raw)

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles dir = do
  entries <- sort <$> listDirectory dir
  concat
    <$> mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else
              if map toLower (takeExtension path) `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  case splitAt (length fixtureRoot + 1) path of
    (prefix, rest)
      | prefix == fixtureRoot <> "/" -> rest
    _ -> path
