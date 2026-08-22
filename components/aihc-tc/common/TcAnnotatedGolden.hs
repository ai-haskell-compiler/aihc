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
  ( Extension (ImplicitPrelude),
    LanguageEdition (Haskell98Edition),
    Module,
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    parseExtensionName,
    parseLanguageEdition,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (ModuleExports, Package (..), PackageId (..), ResolveResult (..), extractInterface, modulesInPackage, resolveWithDeps)
import Aihc.Tc
  ( TcConfig,
    TcInterface (..),
    emptyTcInterface,
    tcConfig,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
    typecheckModulesWithInterface,
  )
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)
import TcAnnotatedRender (renderAnnotatedTcResults)

data ExpectedStatus
  = StatusPass
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

data PrimitiveSupport = PrimitiveSupport
  { supportScopes :: !ModuleExports,
    supportTcInterface :: !TcInterface
  }

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/annotated"

testTcConfig :: TcConfig
testTcConfig = tcConfig (PackageId "aihc-prim")

loadTcAnnotatedCases :: IO [TcAnnotatedCase]
loadTcAnnotatedCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      primitiveSupport `seq` pure ()
      paths <- listFixtureFiles fixtureRoot
      mapM loadTcAnnotatedCase paths

primitiveSupport :: PrimitiveSupport
primitiveSupport = unsafePerformIO $ do
  primitiveModules <- loadPrimitiveModules
  case preparePrimitiveSupport primitiveModules of
    Left errMsg -> fail errMsg
    Right support -> pure support
{-# NOINLINE primitiveSupport #-}

loadPrimitiveModules :: IO [(FilePath, Text)]
loadPrimitiveModules = do
  sourceRoot <- findPrimitiveSourceRoot
  mapM (loadOne sourceRoot) ["GHC/Classes.hs", "GHC/Types.hs", "GHC/Prim.hs", "GHC/Tuple.hs"]
  where
    loadOne sourceRoot relativePath = do
      let path = sourceRoot </> relativePath
      source <- TIO.readFile path
      pure (path, source)

findPrimitiveSourceRoot :: IO FilePath
findPrimitiveSourceRoot = getCurrentDirectory >>= findUp
  where
    findUp directory = do
      let candidate = directory </> "core-libs/aihc-prim/src"
          files = [candidate </> "GHC/Classes.hs", candidate </> "GHC/Types.hs", candidate </> "GHC/Prim.hs", candidate </> "GHC/Tuple.hs"]
      exists <- and <$> mapM doesFileExist files
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail "Cannot find the aihc-prim source modules."
            else findUp parent

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

evaluateTcAnnotatedCase :: TcAnnotatedCase -> IO (Outcome, String)
evaluateTcAnnotatedCase tc = do
  result <- try (evaluate (forceEvaluation (evaluateTcAnnotatedCasePure tc))) :: IO (Either ErrorCall (Outcome, String))
  pure $
    case result of
      Left exception -> classifyFailure tc ("exception: " <> displayException exception)
      Right outcome -> outcome

forceEvaluation :: (Outcome, String) -> (Outcome, String)
forceEvaluation result@(outcome, details) = outcome `seq` length details `seq` result

evaluateTcAnnotatedCasePure :: TcAnnotatedCase -> (Outcome, String)
evaluateTcAnnotatedCasePure tc =
  let parsedModules = map parseOne (caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> classifyFailure tc ("parse error: " <> errMsg)
        Right modules ->
          case resolveWithDeps (supportScopes primitiveSupport) (modulesInPackage fixturePackage modules) of
            ResolveResult {resolvedModules, resolveErrors = []} ->
              let (results, _) =
                    typecheckModulesWithInterface
                      testTcConfig
                      (supportTcInterface primitiveSupport)
                      (map snd resolvedModules)
                  actual = renderAnnotatedTcResults (caseModules tc) results
               in classifySuccess tc actual
            ResolveResult {resolveErrors} ->
              classifyFailure tc ("resolve error: " <> show resolveErrors)
  where
    parseOne input =
      parseModuleText (T.unpack (T.takeWhile (/= '\n') input)) (caseExtensions tc) input

preparePrimitiveSupport :: [(FilePath, Text)] -> Either String PrimitiveSupport
preparePrimitiveSupport primitiveModules =
  case mapM (uncurry parsePrimitiveModule) primitiveModules of
    Left errMsg -> Left ("parse error: " <> errMsg)
    Right modules ->
      case resolveWithDeps mempty (modulesInPackage primitivePackage modules) of
        resolved@ResolveResult {resolvedModules, resolveErrors = []} ->
          let primitiveAsts = map snd resolvedModules
              (primitiveTcResults, tcInterface) = typecheckModuleSccWithInterface testTcConfig emptyTcInterface primitiveAsts
           in if all tcModuleSuccess primitiveTcResults
                then
                  Right
                    PrimitiveSupport
                      { supportScopes = extractInterface resolved,
                        supportTcInterface = tcInterface
                      }
                else Left ("typecheck error: " <> unlines [show d | r <- primitiveTcResults, d <- tcModuleDiagnostics r])
        ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)

primitivePackage :: Package
primitivePackage = Package "aihc-prim" (PackageId "aihc-prim")

fixturePackage :: Package
fixturePackage = Package "" (PackageId "")

parsePrimitiveModule :: FilePath -> Text -> Either String Module
parsePrimitiveModule sourceName input =
  parseModuleText sourceName (primitiveExtensions input) input

parseModuleText :: FilePath -> [Extension] -> Text -> Either String Module
parseModuleText sourceName extensions input =
  let config =
        defaultConfig
          { parserSourceName = sourceName,
            parserExtensions = extensions
          }
      (errs, ast) = parseModule config input
   in if null errs
        then Right ast
        else Left (show errs)

primitiveExtensions :: Text -> [Extension]
primitiveExtensions source =
  filter (/= ImplicitPrelude) (effectiveExtensions language (headerExtensionSettings header))
  where
    header = readModuleHeaderPragmas source
    defaultLanguage = fromMaybe Haskell98Edition (parseLanguageEdition "GHC2021")
    language = fromMaybe defaultLanguage (headerLanguageEdition header)

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
