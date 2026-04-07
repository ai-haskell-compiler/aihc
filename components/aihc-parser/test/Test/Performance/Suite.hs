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
import Data.Char (chr, toLower)
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
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

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

generatedCaseSize :: Int
generatedCaseSize = 1000

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
mkPerfCaseTest perfCase = case perfCaseStatus perfCase of
  StatusXFail -> testCaseInfo (perfCaseId perfCase) (assertPerfCase perfCase >> pure "Known failure - to be fixed")
  _ -> testCase (perfCaseId perfCase) (assertPerfCase perfCase)

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
    (StatusXFail, Nothing) -> pure ()
    (StatusXFail, Just (errs, _))
      | null errs ->
          assertFailure
            ( "Unexpected pass in xfail performance case "
                <> perfCaseId perfCase
                <> " reason="
                <> perfCaseReason perfCase
            )
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

generatedPerfCases :: [PerfCase]
generatedPerfCases =
  [ mkGeneratedPerfCase "tuple-expression-nested" (mkExprModule (nestedTupleExpr generatedCaseSize)),
    mkGeneratedPerfCase "tuple-expression-wide" (mkExprModule (wideTupleExpr generatedCaseSize)),
    mkGeneratedPerfCase "expression-list" (mkExprModule (longListExpr generatedCaseSize)),
    mkGeneratedPerfCase "tuple-type-nested" (mkTypeModule (nestedTupleType generatedCaseSize)),
    mkGeneratedPerfCase "tuple-type-wide" (mkTypeModule (wideTupleType generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-nested" (mkPatternModule (nestedTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-wide" (mkPatternModule (wideTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-function-nested" (mkTuplePatternFunctionModule (nestedTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-function-wide" (mkTuplePatternFunctionModule (wideTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "enum-data-constructors" (mkDataModule (enumDataDecl generatedCaseSize)),
    mkGeneratedPerfCase "record-data-fields" (mkDataModule (recordDataDecl generatedCaseSize)),
    mkGeneratedPerfCase "type-right-leaning-terms" (mkTypeModule (rightLeaningType generatedCaseSize)),
    mkGeneratedPerfCase "type-left-leaning-terms" (mkTypeModule (leftLeaningType generatedCaseSize)),
    mkGeneratedPerfCase "type-parameters" (mkTypeModule (typeWithParameters generatedCaseSize)),
    mkGeneratedPerfCase "string-escapes" (mkExprModule (escapedStringExpr (generatedCaseSize * 100))),
    mkGeneratedPerfCase "nested-application" (mkExprModule (nestedAppExpr generatedCaseSize))
  ]

mkGeneratedPerfCase :: String -> Text -> PerfCase
mkGeneratedPerfCase label inputText =
  mkGeneratedPerfCaseWithStatus label inputText StatusPass ""

mkGeneratedPerfCaseWithStatus :: String -> Text -> ExpectedStatus -> String -> PerfCase
mkGeneratedPerfCaseWithStatus label inputText status reason =
  let caseId = "generated/" <> label <> "-" <> show generatedCaseSize <> ".hs"
   in PerfCase
        { perfCaseId = caseId,
          perfCaseSourceName = caseId,
          perfCaseExtensions = [],
          perfCaseInput = inputText,
          perfCaseStatus = status,
          perfCaseReason = reason
        }

mkExprModule :: Text -> Text
mkExprModule expr = T.unlines ["module Generated where", "value = " <> expr]

mkTypeModule :: Text -> Text
mkTypeModule ty = T.unlines ["module Generated where", "value :: " <> ty, "value = undefined"]

mkPatternModule :: Text -> Text
mkPatternModule pat = T.unlines ["module Generated where", "value " <> pat <> " = x1", "  where", "    x1 = 1"]

mkTuplePatternFunctionModule :: Text -> Text
mkTuplePatternFunctionModule pat = T.unlines ["module Generated where", "fn " <> pat <> " = ()"]

mkDataModule :: Text -> Text
mkDataModule decl = T.unlines ["module Generated where", decl]

nestedTupleExpr :: Int -> Text
nestedTupleExpr n =
  case n of
    0 -> "a"
    _ -> "(" <> "a, " <> nestedTupleExpr (n - 1) <> ")"

wideTupleExpr :: Int -> Text
wideTupleExpr n = tupleText n "a"

longListExpr :: Int -> Text
longListExpr n = "[" <> T.intercalate ", " (replicate n "a") <> "]"

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

escapedStringExpr :: Int -> Text
escapedStringExpr desiredLength =
  T.concat ["\"", takeEscapedText desiredLength escapedStringFragments, "\""]

escapedStringFragments :: [Text]
escapedStringFragments =
  [shownCharBody (chr n) | n <- [0 .. 255]]
  where
    shownCharBody ch =
      let shown = show [ch]
       in T.pack (take (length shown - 2) (drop 1 shown))

takeEscapedText :: Int -> [Text] -> Text
takeEscapedText desiredLength = go 0
  where
    go accLen (fragment : rest)
      | accLen >= desiredLength = ""
      | otherwise =
          fragment <> go (accLen + T.length fragment) rest
    go _ [] = ""

tupleText :: Int -> Text -> Text
tupleText n atom =
  tupleItemsText (replicate n atom)

tupleItemsText :: [Text] -> Text
tupleItemsText items =
  "(" <> T.intercalate ", " items <> ")"

patternVars :: Int -> [Text]
patternVars n = [T.pack ("x" <> show ix) | ix <- [1 .. n]]

-- | Generate deeply nested constructor application: A(A(A(...A(a)...)))
-- This exercises the paren expression parser's performance under deep nesting.
nestedAppExpr :: Int -> Text
nestedAppExpr n =
  case n of
    0 -> "a"
    _ -> "A(" <> nestedAppExpr (n - 1) <> ")"
