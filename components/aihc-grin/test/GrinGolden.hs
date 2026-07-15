{-# LANGUAGE OverloadedStrings #-}

-- | Golden fixtures for source-to-GRIN lowering.
module GrinGolden
  ( ExpectedStatus (..),
    Outcome (..),
    GrinCase (..),
    loadGrinCases,
    evaluateGrinCase,
  )
where

import Aihc.Fc (DesugarResult (..), desugarModuleWithBindings)
import Aihc.Grin (lintProgram, lowerProgram, renderProgram)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax (Extension, Module, parseExtensionName)
import Aihc.Resolve (ResolveResult (..), resolve)
import Aihc.Tc (TcBindingResult, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheck)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))

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

data GrinCase = GrinCase
  { caseId :: !String,
    caseSource :: !Text,
    caseExtensions :: ![Extension],
    caseExpected :: !String,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

loadGrinCases :: IO [GrinCase]
loadGrinCases = do
  root <- grinFixtureRoot
  paths <- listFixtureFiles root
  mapM (loadGrinCase root) paths

grinFixtureRoot :: IO FilePath
grinFixtureRoot = getCurrentDirectory >>= findUp
  where
    findUp directory = do
      let candidate = directory </> "test" </> "Test" </> "Fixtures" </> "grin"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail ("GRIN fixture root does not exist: " <> candidate)
            else findUp parent

loadGrinCase :: FilePath -> FilePath -> IO GrinCase
loadGrinCase root path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left err -> fail ("Invalid YAML GRIN fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value ->
      case parseGrinFixture root path value of
        Left err -> fail err
        Right fixture -> pure fixture

parseGrinFixture :: FilePath -> FilePath -> Y.Value -> Either String GrinCase
parseGrinFixture root path value = do
  (extensionNames, source, expectedText, statusText, reasonText) <-
    parseEither
      ( withObject "GRIN fixture" $ \object -> do
          extensions <- object .:? "extensions" .!= []
          source <- object .: "source"
          expected <- object .:? "expected" .!= ""
          status <- object .: "status"
          reason <- object .:? "reason" .!= ""
          pure (extensions, source, expected, status, reason)
      )
      value
  extensions <- traverse (validateExtension path) extensionNames
  status <- parseStatus path statusText
  pure
    GrinCase
      { caseId = makeRelative root path,
        caseSource = source,
        caseExtensions = extensions,
        caseExpected = trim (T.unpack expectedText),
        caseStatus = status,
        caseReason = trim (T.unpack reasonText)
      }

evaluateGrinCase :: GrinCase -> (Outcome, String)
evaluateGrinCase fixture =
  case parseSource fixture of
    Left err -> classifyFailure fixture ("parse error: " <> err)
    Right parsed ->
      case resolve [parsed] of
        ResolveResult {resolvedModules, resolveErrors = []} ->
          let tcResults = typecheck resolvedModules
           in if all tcModuleSuccess tcResults
                then
                  let allBindings = moduleGroupBindings tcResults
                      desugared = zipWith (desugarModuleWithBindings allBindings) tcResults resolvedModules
                   in if all dsSuccess desugared
                        then
                          let programs = map (lowerProgram . dsProgram) desugared
                              lintErrors = concatMap lintProgram programs
                           in if null lintErrors
                                then classifySuccess fixture (unlines (map renderProgram programs))
                                else classifyFailure fixture ("GRIN lint error: " <> show lintErrors)
                        else classifyFailure fixture ("desugar error: " <> unlines (concatMap dsErrors desugared))
                else classifyFailure fixture ("typecheck error: " <> renderTcErrors tcResults)
        ResolveResult {resolveErrors} -> classifyFailure fixture ("resolve error: " <> show resolveErrors)

parseSource :: GrinCase -> Either String Module
parseSource fixture =
  let config =
        defaultConfig
          { parserSourceName = caseId fixture,
            parserExtensions = caseExtensions fixture
          }
      (errors, parsed) = parseModule config (caseSource fixture)
   in if null errors then Right parsed else Left (show errors)

moduleGroupBindings :: [Module] -> [TcBindingResult]
moduleGroupBindings = concatMap tcModuleBindings

renderTcErrors :: [Module] -> String
renderTcErrors results = unlines [show diagnostic | result <- results, diagnostic <- tcModuleDiagnostics result]

classifySuccess :: GrinCase -> String -> (Outcome, String)
classifySuccess fixture actual =
  case caseStatus fixture of
    StatusPass
      | trim actual == caseExpected fixture -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "output mismatch\nexpected:\n" <> caseExpected fixture <> "\nactual:\n" <> trim actual
          )
    StatusFail -> (OutcomeFail, "expected failure but GRIN lowering succeeded")
    StatusXFail
      | trim actual == caseExpected fixture -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | trim actual == caseExpected fixture -> (OutcomeXPass, "known bug still passes")
      | otherwise -> (OutcomeFail, "expected xpass output match but got:\n" <> trim actual)

classifyFailure :: GrinCase -> String -> (Outcome, String)
classifyFailure fixture details =
  case caseStatus fixture of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> details)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> details)

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles directory = do
  exists <- doesDirectoryExist directory
  if not exists
    then fail ("GRIN fixture root does not exist: " <> directory)
    else do
      entries <- sort <$> listDirectory directory
      concat
        <$> mapM
          ( \entry -> do
              let path = directory </> entry
              isDirectory <- doesDirectoryExist path
              if isDirectory
                then listFixtureFiles path
                else pure [path | map toLower (takeExtension path) `elem` [".yaml", ".yml"]]
          )
          entries

validateExtension :: FilePath -> Text -> Either String Extension
validateExtension path name =
  maybe (Left ("Unknown extension " <> show name <> " in " <> path)) Right (parseExtensionName name)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
