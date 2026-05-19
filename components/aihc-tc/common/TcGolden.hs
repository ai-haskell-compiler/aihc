{-# LANGUAGE OverloadedStrings #-}

-- | Golden test infrastructure for the type checker.
--
-- Loads YAML fixtures from @test/Test/Fixtures/golden/@, parses
-- the module sources, runs the type checker, and compares the
-- inferred binding types against the expected output.
module TcGolden
  ( ExpectedStatus (..),
    Outcome (..),
    TcCase (..),
    fixtureRoot,
    annotationFixtureRoot,
    loadTcCases,
    loadTcAnnotationCases,
    evaluateTcCase,
    evaluateTcAnnotationCase,
    progressSummary,
    TcAnnotationCase (..),
  )
where

import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    parseModule,
  )
import Aihc.Parser.Syntax (Extension, parseExtensionName)
import Aihc.Tc (TcBindingResult (..), TcModuleResult (..), renderTcType, typecheck)
import Aihc.Tc.Annotations (renderAnnotatedTcModules, renderTcAnnotations)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

data ExpectedStatus
  = StatusPass
  | StatusFail
  | StatusXPass
  | StatusXFail
  deriving (Eq, Show)

data Outcome
  = OutcomePass
  | OutcomeXFail
  | OutcomeXPass
  | OutcomeFail
  deriving (Eq, Show)

data TcCase = TcCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseExpected :: !String,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

data TcAnnotationCase = TcAnnotationCase
  { annotationCaseId :: !String,
    annotationCaseCategory :: !String,
    annotationCasePath :: !FilePath,
    annotationCaseExtensions :: ![Extension],
    annotationCaseModules :: ![Text],
    annotationCaseExpected :: !String,
    annotationCaseAnnotated :: ![String],
    annotationCaseStatus :: !ExpectedStatus,
    annotationCaseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/golden"

annotationFixtureRoot :: FilePath
annotationFixtureRoot = "test/Test/Fixtures/annotations"

loadTcCases :: IO [TcCase]
loadTcCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadTcCase paths

loadTcAnnotationCases :: IO [TcAnnotationCase]
loadTcAnnotationCases = do
  exists <- doesDirectoryExist annotationFixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles annotationFixtureRoot
      mapM loadTcAnnotationCase paths

loadTcCase :: FilePath -> IO TcCase
loadTcCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseTcFixture path value of
      Left e -> fail e
      Right c -> pure c

loadTcAnnotationCase :: FilePath -> IO TcAnnotationCase
loadTcAnnotationCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseTcAnnotationFixture path value of
      Left e -> fail e
      Right c -> pure c

parseTcFixture :: FilePath -> Y.Value -> Either String TcCase
parseTcFixture path value = do
  (extNames, modules, expectedText, statusText, reasonText) <-
    parseEither
      ( withObject "tc fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          expected <- (obj .:? "expected" >>= traverse parseExpectedValue) .!= ""
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, expected, status, reason)
      )
      value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
      expected = trim (T.unpack expectedText)
      reason = trim (T.unpack reasonText)
  pure
    TcCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseModules = modules,
        caseExpected = expected,
        caseStatus = status,
        caseReason = reason
      }

parseTcAnnotationFixture :: FilePath -> Y.Value -> Either String TcAnnotationCase
parseTcAnnotationFixture path value = do
  (extNames, modules, expectedText, annotatedTexts, statusText, reasonText) <-
    parseEither
      ( withObject "tc annotation fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          expected <- (obj .:? "expected" >>= traverse parseExpectedValue) .!= ""
          annotated <- obj .: "annotated" >>= parseAnnotatedList
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, expected, annotated, status, reason)
      )
      value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  let relPath = dropRootPrefixFrom annotationFixtureRoot path
      category = categoryFromPath relPath
      reason = trim (T.unpack reasonText)
  expected <- validateAnnotationExpected path status (T.unpack expectedText)
  annotated <- validateAnnotationAnnotated path status (map (trim . T.unpack) annotatedTexts)
  reason' <- validateAnnotationReason path status reason
  pure
    TcAnnotationCase
      { annotationCaseId = relPath,
        annotationCaseCategory = category,
        annotationCasePath = relPath,
        annotationCaseExtensions = exts,
        annotationCaseModules = modules,
        annotationCaseExpected = expected,
        annotationCaseAnnotated = annotated,
        annotationCaseStatus = status,
        annotationCaseReason = reason'
      }

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

parseAnnotatedList :: Y.Value -> Y.Parser [Text]
parseAnnotatedList = withArray "annotated" $ \arr ->
  mapM parseAnnotatedEntry (foldr (:) [] arr)
  where
    parseAnnotatedEntry (Y.String t) = pure t
    parseAnnotatedEntry _ = fail "each annotated entry must be a string"

parseExpectedValue :: Y.Value -> Y.Parser Text
parseExpectedValue (Y.String txt) = pure txt
parseExpectedValue (Y.Array arr) = T.intercalate "\n" <$> mapM parseLine (foldr (:) [] arr)
  where
    parseLine (Y.String t) = pure t
    parseLine _ = fail "each expected line must be a string"
parseExpectedValue (Y.Object obj) =
  T.intercalate "\n"
    <$> mapM parseModExpected (sortOn (Key.toText . fst) (KeyMap.toList obj))
  where
    parseModExpected (modName, modVal) = do
      lines' <- parseModLines modVal
      pure (Key.toText modName <> ":\n" <> T.intercalate "\n" (map ("  " <>) lines'))
    parseModLines (Y.String t) = pure [t]
    parseModLines (Y.Array arr) = mapM parseLine (foldr (:) [] arr)
    parseModLines _ = fail "module expected value must be a string or list"
    parseLine (Y.String t) = pure t
    parseLine _ = fail "each expected line must be a string"
parseExpectedValue _ = fail "expected must be a string, list, or object"

evaluateTcCase :: TcCase -> (Outcome, String)
evaluateTcCase tc =
  let parsedModules = map parseOne (caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> classifyFailure tc ("parse error: " <> errMsg)
        Right modules ->
          let results = typecheck modules
           in if all tcmSuccess results
                then classifySuccess tc (renderResults results)
                else classifyFailure tc (renderDiags results)
  where
    parseOne input =
      let config =
            defaultConfig
              { parserSourceName = T.unpack (T.takeWhile (/= '\n') input),
                parserExtensions = caseExtensions tc
              }
          (errs, ast) = parseModule config input
       in if null errs
            then Right ast
            else Left (show errs)
    renderResults results =
      let bindings = concatMap tcmBindings results
       in unlines [T.unpack (tbDisplayName b) <> " :: " <> renderTcType (tbType b) | b <- bindings]
    renderDiags results =
      unlines [show d | r <- results, d <- tcmDiagnostics r]

evaluateTcAnnotationCase :: TcAnnotationCase -> (Outcome, String)
evaluateTcAnnotationCase tc =
  let parsedModules = map parseOne (annotationCaseModules tc)
   in case sequence parsedModules of
        Left errMsg -> classifyAnnotationFailure tc ("parse error: " <> errMsg)
        Right modules ->
          let results = typecheck modules
           in if all tcmSuccess results
                then classifyAnnotationSuccess tc (renderAnnotationResults results) (renderAnnotatedResults results)
                else classifyAnnotationFailure tc (renderDiags results)
  where
    parseOne input =
      let config =
            defaultConfig
              { parserSourceName = T.unpack (T.takeWhile (/= '\n') input),
                parserExtensions = annotationCaseExtensions tc
              }
          (errs, ast) = parseModule config input
       in if null errs
            then Right ast
            else Left (show errs)
    renderAnnotationResults results =
      trim (renderTcAnnotations (map tcmModule results))
    renderAnnotatedResults results =
      map trim (renderAnnotatedTcModules (annotationCaseModules tc) (map tcmModule results))
    renderDiags results =
      unlines [show d | r <- results, d <- tcmDiagnostics r]

classifySuccess :: TcCase -> String -> (Outcome, String)
classifySuccess tc actual =
  case caseStatus tc of
    StatusPass
      | trim actual == trim (caseExpected tc) -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "output mismatch\nexpected: " <> show (caseExpected tc) <> "\nactual:   " <> show (trim actual)
          )
    StatusFail ->
      (OutcomeFail, "expected failure but TC succeeded")
    StatusXFail
      | trim actual == trim (caseExpected tc) -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | trim actual == trim (caseExpected tc) -> (OutcomeXPass, "known bug still passes")
      | otherwise ->
          (OutcomeFail, "expected xpass output match but got: " <> trim actual)

classifyFailure :: TcCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case caseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

classifyAnnotationSuccess :: TcAnnotationCase -> String -> [String] -> (Outcome, String)
classifyAnnotationSuccess tc actual actualAnnotated =
  case annotationCaseStatus tc of
    StatusPass
      | actual /= annotationCaseExpected tc ->
          ( OutcomeFail,
            "annotation output mismatch\nexpected:\n" <> annotationCaseExpected tc <> "\nactual:\n" <> actual
          )
      | actualAnnotated /= annotationCaseAnnotated tc ->
          ( OutcomeFail,
            "annotated source mismatch\nexpected:\n" <> unlines (annotationCaseAnnotated tc) <> "\nactual:\n" <> unlines actualAnnotated
          )
      | otherwise -> (OutcomePass, "")
    StatusFail ->
      (OutcomeFail, "expected failure but TC annotation case succeeded")
    StatusXFail
      | outputMatches -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | outputMatches -> (OutcomeXPass, "known bug still passes")
      | otherwise ->
          (OutcomeFail, "expected xpass output match but got annotations:\n" <> actual)
  where
    outputMatches =
      actual == annotationCaseExpected tc
        && actualAnnotated == annotationCaseAnnotated tc

classifyAnnotationFailure :: TcAnnotationCase -> String -> (Outcome, String)
classifyAnnotationFailure tc errDetails =
  case annotationCaseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

progressSummary :: [(TcCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

-- Utilities

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
              if takeExtension path `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

validateExtensions :: FilePath -> [Text] -> Either String [Extension]
validateExtensions path = traverse parseOne
  where
    parseOne raw =
      case parseExtensionName raw of
        Just ext -> Right ext
        Nothing -> Left ("Unknown extension " <> show raw <> " in " <> path)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

validateAnnotationReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateAnnotationReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
        _ -> Right trimmed

validateAnnotationExpected :: FilePath -> ExpectedStatus -> String -> Either String String
validateAnnotationExpected path status expected =
  let trimmed = trim expected
   in case status of
        StatusPass | null trimmed -> Left ("[expected] is required for pass status in " <> path)
        StatusXPass | null trimmed -> Left ("[expected] is required for xpass status in " <> path)
        _ -> Right trimmed

validateAnnotationAnnotated :: FilePath -> ExpectedStatus -> [String] -> Either String [String]
validateAnnotationAnnotated path status annotated =
  let trimmed = map trim annotated
   in case status of
        StatusPass | null trimmed || all null trimmed -> Left ("[annotated] is required for pass status in " <> path)
        StatusXPass | null trimmed || all null trimmed -> Left ("[annotated] is required for xpass status in " <> path)
        _ -> Right trimmed

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix = dropRootPrefixFrom fixtureRoot

dropRootPrefixFrom :: FilePath -> FilePath -> FilePath
dropRootPrefixFrom root path =
  maybe path T.unpack (T.stripPrefix (T.pack (root <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "golden"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
