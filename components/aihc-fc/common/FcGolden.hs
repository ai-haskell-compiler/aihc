{-# LANGUAGE OverloadedStrings #-}

-- | Golden test infrastructure for System FC desugaring.
--
-- Loads YAML fixtures from @test/Test/Fixtures/golden/@, parses
-- the module sources, runs type checking and desugaring, and compares
-- the pretty-printed Core output against expected output.
module FcGolden
  ( ExpectedStatus (..),
    Outcome (..),
    FcCase (..),
    fixtureRoot,
    loadFcCases,
    evaluateFcCase,
    updateFcGoldens,
  )
where

import Aihc.Fc.Desugar (DesugarConfig (..), DesugarResult (..), desugarModuleWithInterface)
import Aihc.Fc.Parser (parseProgram, renderParseError)
import Aihc.Fc.Pretty (renderProgram)
import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    parseModule,
  )
import Aihc.Parser.Syntax (Extension, Module, moduleName, parseExtensionName)
import Aihc.Resolve (Package (..), PackageId (..), ResolveResult (..), resolveWithDeps)
import Aihc.Tc (TcBindingResult, emptyTcInterface, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModulesWithInterface)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, isPrefixOf, sort)
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

data FcCase = FcCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseSupportModules :: ![Text],
    caseModules :: ![Text],
    caseExpected :: !String,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/golden"

loadFcCases :: IO [FcCase]
loadFcCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM (loadFcCase [listSupportModule, tupleSupportModule]) paths

-- Golden modules deliberately omit core-library dependencies. Keep their
-- primitive tuple support source-defined while limiting the fixture to the
-- arities exercised here; integration fixtures load the real aihc-prim module.
tupleSupportModule :: Text
tupleSupportModule =
  T.unlines
    [ "module GHC.Tuple where",
      "data Unit = ()",
      "data Tuple2 a b = (a, b)"
    ]

listSupportModule :: Text
listSupportModule =
  T.unlines
    [ "module GHC.Types (Bool(..), List(..), Type, TYPE) where",
      "data Bool = False | True",
      "data List a = [] | a : [a]",
      "data Type",
      "data TYPE rep",
      "infixr 5 :"
    ]

loadFcCase :: [Text] -> FilePath -> IO FcCase
loadFcCase supportModules path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseFcFixture supportModules path value of
      Left e -> fail e
      Right c -> pure c

parseFcFixture :: [Text] -> FilePath -> Y.Value -> Either String FcCase
parseFcFixture supportModules path value = do
  (extNames, modules, expectedText, statusText, reasonText) <-
    parseEither
      ( withObject "fc fixture" $ \obj -> do
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
    FcCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseSupportModules = supportModules,
        caseModules = modules,
        caseExpected = expected,
        caseStatus = status,
        caseReason = reason
      }

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

parseExpectedValue :: Y.Value -> Y.Parser Text
parseExpectedValue (Y.String txt) = pure txt
parseExpectedValue (Y.Array arr) = T.intercalate "\n" <$> mapM parseLine (foldr (:) [] arr)
  where
    parseLine (Y.String t) = pure t
    parseLine _ = fail "each expected line must be a string"
parseExpectedValue _ = fail "expected must be a string or list"

evaluateFcCase :: FcCase -> (Outcome, String)
evaluateFcCase tc =
  case renderFcCase tc of
    Left details -> classifyFailure tc details
    Right actual -> classifySuccess tc actual

renderFcCase :: FcCase -> Either String String
renderFcCase tc =
  let supportModuleCount = length supportModules
      parsedModules = map parseOne (supportModules <> caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> Left ("parse error: " <> errMsg)
        Right modules ->
          case resolveWithDeps mempty (zipWith modulePackage [0 :: Int ..] modules) of
            ResolveResult {resolvedModules, resolveErrors = []} ->
              let moduleAsts = map snd resolvedModules
                  (tcResults, tcInterface) = typecheckModulesWithInterface emptyTcInterface moduleAsts
               in if all tcModuleSuccess tcResults
                    then
                      let allBindings = moduleGroupBindings tcResults
                          results =
                            map
                              (desugarModuleWithInterface (DesugarConfig {primPackageId = PackageId "aihc-prim"}) allBindings tcInterface)
                              tcResults
                          fixtureResults = drop supportModuleCount results
                       in if all dsSuccess results
                            then renderResults fixtureResults
                            else Left (renderErrors results)
                    else Left ("typecheck error: " <> renderTcErrors tcResults)
            ResolveResult {resolveErrors} ->
              Left ("resolve error: " <> show resolveErrors)
  where
    hasFixtureGhcTypes = any (T.isPrefixOf "module GHC.Types" . T.stripStart) (caseModules tc)
    supportModules = if hasFixtureGhcTypes then drop 1 (caseSupportModules tc) else caseSupportModules tc
    modulePackage _ modu
      | moduleName modu `elem` [Just "GHC.Classes", Just "GHC.Prim", Just "GHC.Types"] =
          (Package "aihc-prim" (PackageId "aihc-prim"), modu)
      | otherwise = (Package "" (PackageId ""), modu)
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
      unlines <$> traverse renderResult results
    renderResult result =
      let rendered = renderProgram (dsProgram result)
       in case parseProgram (T.pack rendered) of
            Left parseError -> Left ("System FC round-trip parse error:\n" <> renderParseError parseError <> "\n" <> rendered)
            Right parsed ->
              let canonical = renderProgram parsed
               in if canonical == rendered
                    then Right rendered
                    else Left ("System FC round trip changed canonical syntax:\n" <> canonical <> "\noriginal:\n" <> rendered)
    renderErrors results =
      unlines [err | r <- results, err <- dsErrors r]

-- | Refresh passing FC fixtures with the current canonical rendering while
-- preserving their source and metadata layout.
updateFcGoldens :: IO ()
updateFcGoldens = do
  cases <- loadFcCases
  mapM_ updateCase [testCase | testCase <- cases, caseStatus testCase == StatusPass]
  where
    updateCase testCase =
      case renderFcCase testCase of
        Left details -> fail (caseId testCase <> ": " <> details)
        Right actual -> do
          let path = fixtureRoot </> casePath testCase
          contents <- readFile path
          let updated = replaceExpected actual contents
          length updated `seq` writeFile path updated

    replaceExpected actual contents =
      let sourceLines = lines contents
          (beforeExpected, fromExpected) = break ("expected:" `isPrefixOf`) sourceLines
          (beforeStatus, fromStatus) = break ("status:" `isPrefixOf`) sourceLines
          renderedExpected = "expected: |-" : map indentExpectedLine (lines (trim actual))
          resultLines =
            case fromExpected of
              [] -> beforeStatus <> renderedExpected <> fromStatus
              _ : afterExpected ->
                beforeExpected <> renderedExpected <> dropWhile isExpectedBody afterExpected
       in unlines resultLines

    isExpectedBody line =
      case line of
        [] -> True
        first : _ -> isSpace first

    indentExpectedLine line
      | null line = ""
      | otherwise = "  " <> line

moduleGroupBindings :: [Module] -> [TcBindingResult]
moduleGroupBindings =
  concatMap tcModuleBindings

renderTcErrors :: [Module] -> String
renderTcErrors results =
  unlines [show d | r <- results, d <- tcModuleDiagnostics r]

classifySuccess :: FcCase -> String -> (Outcome, String)
classifySuccess tc actual =
  case caseStatus tc of
    StatusPass
      | trim actual == trim (caseExpected tc) -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "output mismatch\nexpected:\n" <> caseExpected tc <> "\nactual:\n" <> trim actual
          )
    StatusFail ->
      (OutcomeFail, "expected failure but desugaring succeeded")
    StatusXFail
      | trim actual == trim (caseExpected tc) -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | trim actual == trim (caseExpected tc) -> (OutcomeXPass, "known bug still passes")
      | otherwise ->
          (OutcomeFail, "expected xpass output match but got: " <> trim actual)

classifyFailure :: FcCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case caseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

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

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "golden"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
