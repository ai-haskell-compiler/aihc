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
    perfCaseTarget :: !PerfTarget,
    perfCaseSourceName :: !FilePath,
    perfCaseExtensions :: ![Extension],
    perfCaseInput :: !Text,
    perfCaseStatus :: !ExpectedStatus,
    perfCaseReason :: !String
  }

data PerfTarget
  = PerfModule
  | PerfExpr
  | PerfType
  | PerfPattern

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/performance/module"

timeoutMicros :: Int
timeoutMicros = 1000000

nestedTupleExprSize :: Int
nestedTupleExprSize = 4

generatedCaseSize :: Int
generatedCaseSize = 100

parserPerformanceTests :: IO TestTree
parserPerformanceTests = do
  fixtureCases <- loadPerfCases
  pure $
    testGroup
      "performance"
      [ testGroup
          "parse-under-1s"
          [ testGroup "fixtures" (map mkPerfCaseTest fixtureCases),
            testGroup "generated" (map mkPerfCaseTest generatedPerfCases)
          ]
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
          runPerfCase perfCase
  case (perfCaseStatus perfCase, outcome) of
    (StatusPass, Nothing) ->
      assertFailure
        ( "module parse exceeded "
            <> show timeoutMicros
            <> "us for "
            <> perfCaseId perfCase
        )
    (StatusPass, Just False) ->
      assertFailure
        ("expected parse success for performance case " <> perfCaseId perfCase)
    (StatusXFail, Just True) ->
      assertFailure
        ( "Unexpected pass in xfail performance case "
            <> perfCaseId perfCase
            <> " reason="
            <> perfCaseReason perfCase
        )
    (StatusXFail, _) -> pure ()
    _ -> pure ()

runPerfCase :: PerfCase -> Bool
runPerfCase perfCase =
  let cfg =
        defaultConfig
          { parserSourceName = perfCaseSourceName perfCase,
            parserExtensions = perfCaseExtensions perfCase
          }
   in case perfCaseTarget perfCase of
        PerfModule ->
          let (errs, _) = parseModule cfg (perfCaseInput perfCase)
           in null errs
        PerfExpr ->
          case parseExpr cfg (perfCaseInput perfCase) of
            ParseOk _ -> True
            ParseErr _ -> False
        PerfType ->
          case parseType cfg (perfCaseInput perfCase) of
            ParseOk _ -> True
            ParseErr _ -> False
        PerfPattern ->
          case parsePattern cfg (perfCaseInput perfCase) of
            ParseOk _ -> True
            ParseErr _ -> False

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
        perfCaseTarget = PerfModule,
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

generatedPerfCases :: [PerfCase]
generatedPerfCases =
  [ mkGeneratedPerfCase PerfExpr "tuple-expression-nested" nestedTupleExprSize (nestedTupleExpr nestedTupleExprSize),
    mkGeneratedPerfCase PerfExpr "tuple-expression-wide" generatedCaseSize (wideTupleExpr generatedCaseSize),
    mkGeneratedPerfCase PerfType "tuple-type-nested" generatedCaseSize (nestedTupleType generatedCaseSize),
    mkGeneratedPerfCase PerfType "tuple-type-wide" generatedCaseSize (wideTupleType generatedCaseSize),
    mkGeneratedPerfCase PerfPattern "tuple-pattern-nested" generatedCaseSize (nestedTuplePattern generatedCaseSize),
    mkGeneratedPerfCase PerfPattern "tuple-pattern-wide" generatedCaseSize (wideTuplePattern generatedCaseSize),
    mkGeneratedPerfCase PerfModule "enum-data-constructors" generatedCaseSize (enumDataDecl generatedCaseSize),
    mkGeneratedPerfCase PerfModule "record-data-fields" generatedCaseSize (recordDataDecl generatedCaseSize),
    mkGeneratedPerfCase PerfType "type-right-leaning-terms" generatedCaseSize (rightLeaningType generatedCaseSize),
    mkGeneratedPerfCase PerfType "type-left-leaning-terms" generatedCaseSize (leftLeaningType generatedCaseSize),
    mkGeneratedPerfCase PerfType "type-parameters" generatedCaseSize (typeWithParameters generatedCaseSize)
  ]

mkGeneratedPerfCase :: PerfTarget -> String -> Int -> Text -> PerfCase
mkGeneratedPerfCase target label size inputText =
  let caseId = "generated/" <> label <> "-" <> show size <> ".hs"
   in PerfCase
        { perfCaseId = caseId,
          perfCaseTarget = target,
          perfCaseSourceName = caseId,
          perfCaseExtensions = [],
          perfCaseInput = inputText,
          perfCaseStatus = StatusPass,
          perfCaseReason = ""
        }

nestedTupleExpr :: Int -> Text
nestedTupleExpr n =
  case n of
    0 -> "a"
    _ -> "(" <> "a, " <> nestedTupleExpr (n - 1) <> ")"

wideTupleExpr :: Int -> Text
wideTupleExpr n = tupleText n "a"

nestedTupleType :: Int -> Text
nestedTupleType n =
  case n of
    0 -> "A"
    _ -> "(" <> "A, " <> nestedTupleType (n - 1) <> ")"

wideTupleType :: Int -> Text
wideTupleType n = tupleText n "A"

nestedTuplePattern :: Int -> Text
nestedTuplePattern n =
  case n of
    0 -> "x1"
    _ -> "(" <> T.pack ("x" <> show (n + 1)) <> ", " <> nestedTuplePattern (n - 1) <> ")"

wideTuplePattern :: Int -> Text
wideTuplePattern n = tupleItemsText (patternVars n)

enumDataDecl :: Int -> Text
enumDataDecl n =
  "data T = "
    <> T.intercalate " | " [T.pack ("C" <> show ix) | ix <- [1 .. n]]

recordDataDecl :: Int -> Text
recordDataDecl n =
  "data T = T\n  { "
    <> T.intercalate "\n  , " [T.pack ("field" <> show ix) <> " :: A" | ix <- [1 .. n]]
    <> "\n  }"

rightLeaningType :: Int -> Text
rightLeaningType n =
  case n of
    0 -> "A"
    1 -> "A"
    _ -> "A -> (" <> rightLeaningType (n - 1) <> ")"

leftLeaningType :: Int -> Text
leftLeaningType n =
  case n of
    0 -> "A"
    1 -> "A"
    _ -> "(" <> leftLeaningType (n - 1) <> " -> A)"

typeWithParameters :: Int -> Text
typeWithParameters n =
  T.unwords ("A" : replicate n "a")

tupleText :: Int -> Text -> Text
tupleText n atom =
  tupleItemsText (replicate n atom)

tupleItemsText :: [Text] -> Text
tupleItemsText items =
  "(" <> T.intercalate ", " items <> ")"

patternVars :: Int -> [Text]
patternVars n = [T.pack ("x" <> show ix) | ix <- [1 .. n]]
