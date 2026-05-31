{-# LANGUAGE OverloadedStrings #-}

-- | Inline annotated golden test infrastructure for the type checker.
--
-- This is intentionally separate from the existing TC golden fixtures. The
-- existing fixtures assert the compact top-level signature summary; these
-- fixtures assert a human-readable source overlay for type-checker output.
module TcAnnotatedGolden
  ( ExpectedStatus (..),
    Outcome (..),
    TcAnnotatedCase (..),
    fixtureRoot,
    loadTcAnnotatedCases,
    evaluateTcAnnotatedCase,
    renderAnnotatedTcResults,
  )
where

import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    parseModule,
  )
import Aihc.Parser.Syntax
  ( Extension,
    parseExtensionName,
  )
import Aihc.Tc (TcModuleResult (..), typecheck)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import TcAnnotatedRender (renderAnnotatedTcResults)

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

data TcAnnotatedCase = TcAnnotatedCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseAnnotated :: ![String],
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/annotated"

loadTcAnnotatedCases :: IO [TcAnnotatedCase]
loadTcAnnotatedCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadTcAnnotatedCase paths

loadTcAnnotatedCase :: FilePath -> IO TcAnnotatedCase
loadTcAnnotatedCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseTcAnnotatedFixture path value of
      Left e -> fail e
      Right c -> pure c

parseTcAnnotatedFixture :: FilePath -> Y.Value -> Either String TcAnnotatedCase
parseTcAnnotatedFixture path value = do
  (extNames, modules, annotatedTexts, statusText, reasonText) <-
    parseEither
      ( withObject "tc annotated fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          annotated <- obj .: "annotated" >>= parseAnnotatedList
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, annotated, status, reason)
      )
      value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
      reason = trim (T.unpack reasonText)
      annotated = map (trim . T.unpack) annotatedTexts
  pure
    TcAnnotatedCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseModules = modules,
        caseAnnotated = annotated,
        caseStatus = status,
        caseReason = reason
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

evaluateTcAnnotatedCase :: TcAnnotatedCase -> (Outcome, String)
evaluateTcAnnotatedCase tc =
  let parsedModules = map parseOne (caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> classifyFailure tc ("parse error: " <> errMsg)
        Right modules ->
          let results = typecheck modules
              actual = renderAnnotatedTcResults (caseModules tc) results
              hasTypeErrors = not (all tcmSuccess results)
           in case caseStatus tc of
                StatusFail
                  | hasTypeErrors -> classifyFailure tc (renderDiags results)
                  | otherwise -> classifySuccess tc actual
                StatusXFail
                  | hasTypeErrors -> classifyFailure tc (renderDiags results)
                  | otherwise -> classifySuccess tc actual
                _ -> classifySuccess tc actual
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
    renderDiags results =
      unlines [show d | r <- results, d <- tcmDiagnostics r]

classifySuccess :: TcAnnotatedCase -> [String] -> (Outcome, String)
classifySuccess tc actual =
  let expected = caseAnnotated tc
      outputMatches = map trim actual == map trim expected
   in case caseStatus tc of
        StatusPass
          | outputMatches -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "annotated output mismatch\nexpected:\n"
                  <> unlines expected
                  <> "\nactual:\n"
                  <> unlines actual
              )
        StatusFail ->
          (OutcomeFail, "expected failure but TC succeeded")
        StatusXFail
          | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeXFail, "")
        StatusXPass
          | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeFail, "expected xpass output match")

classifyFailure :: TcAnnotatedCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case caseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

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

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "annotated"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
