{-# LANGUAGE OverloadedStrings #-}

module ResolverGolden
  ( ExpectedStatus (..),
    Outcome (..),
    ResolverCase (..),
    fixtureRoot,
    loadResolverCases,
    parseResolverCaseText,
    evaluateResolverCase,
    progressSummary,
  )
where

import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    formatParseErrors,
    parseModule,
  )
import Aihc.Parser.Syntax (Extension, parseExtensionName)
import Aihc.Resolve (ResolveResult (..), renderAnnotatedResolveResult, renderResolveResult, resolve)
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
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

data ResolverCase = ResolverCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseExpected :: !String,
    caseAnnotated :: ![String],
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/golden"

loadResolverCases :: IO [ResolverCase]
loadResolverCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadResolverCase paths

loadResolverCase :: FilePath -> IO ResolverCase
loadResolverCase path = do
  source <- TIO.readFile path
  case parseResolverCaseText path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseResolverCaseText :: FilePath -> Text -> Either String ResolverCase
parseResolverCaseText path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, modules, expectedText, annotatedTexts, statusText, reasonText) <- parseYamlFixture path value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  expected <- validateExpected path status (T.unpack expectedText)
  annotated <- validateAnnotated path status (map (trim . T.unpack) annotatedTexts)
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    ResolverCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseModules = modules,
        caseExpected = expected,
        caseAnnotated = annotated,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], [Text], Text, [Text], Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "resolver fixture" $ \obj -> do
        exts <- obj .: "extensions"
        modules <- obj .: "modules" >>= parseModules
        expectedText <- (obj .:? "expected" >>= traverse parseExpectedValue) .!= ""
        annotatedTexts <- obj .: "annotated" >>= parseAnnotatedList
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, modules, expectedText, annotatedTexts, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid resolver fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (toList arr)
  where
    toList = foldr (:) []
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

parseAnnotatedList :: Y.Value -> Y.Parser [Text]
parseAnnotatedList = withArray "annotated" $ \arr -> do
  mapM parseAnnotatedEntry (foldr (:) [] arr)
  where
    parseAnnotatedEntry (Y.String t) = pure t
    parseAnnotatedEntry _ = fail "each annotated entry must be a string"

parseExpectedValue :: Y.Value -> Y.Parser Text
parseExpectedValue value =
  case value of
    Y.String txt -> pure txt
    Y.Array arr -> T.intercalate "\n" <$> mapM parseExpectedLine (toList arr)
    Y.Object obj ->
      T.intercalate "\n"
        <$> mapM parseExpectedModule (sortOn (Key.toText . fst) (KeyMap.toList obj))
    _ -> fail "expected must be a string or a list of strings"
  where
    toList = foldr (:) []
    parseExpectedModule (moduleName, moduleValue) = do
      moduleLines <- parseExpectedModuleLines moduleValue
      pure (Key.toText moduleName <> ":\n" <> T.intercalate "\n" (map ("  " <>) moduleLines))
    parseExpectedModuleLines (Y.String txt) = pure [txt]
    parseExpectedModuleLines (Y.Array arr) = mapM parseExpectedLine (toList arr)
    parseExpectedModuleLines _ = fail "each expected module value must be a string or a list of strings"
    parseExpectedLine (Y.String txt) = pure txt
    parseExpectedLine _ = fail "each expected list entry must be a string"

evaluateResolverCase :: ResolverCase -> (Outcome, String)
evaluateResolverCase meta =
  let parsedModules = map parseOne (caseModules meta)
   in case sequence parsedModules of
        Left errMsg -> classifyFailure meta ("parse error: " <> errMsg)
        Right modules ->
          let result = resolve modules
           in if null (resolveErrors result)
                then classifySuccess meta (showResolved result) (showAnnotated result)
                else classifyFailure meta (showErrors result)
  where
    parserConfig input =
      defaultConfig
        { parserSourceName = T.unpack (T.takeWhile (/= '\n') input),
          parserExtensions = caseExtensions meta
        }
    parseOne input =
      let (errs, ast) = parseModule (parserConfig input) input
       in if null errs
            then Right ast
            else Left (formatParseErrors (T.unpack (T.takeWhile (/= '\n') input)) (Just input) errs)
    showResolved = renderResolveResult
    showAnnotated = renderAnnotatedResolveResult (caseModules meta)
    showErrors result = unlines (map show (resolveErrors result))

classifySuccess :: ResolverCase -> String -> [String] -> (Outcome, String)
classifySuccess meta actual actualAnnotated =
  case caseStatus meta of
    StatusPass
      | actual /= caseExpected meta ->
          ( OutcomeFail,
            "expected:\n" <> caseExpected meta <> "\nfound:\n" <> actual
          )
      | not (null (caseAnnotated meta)) && actualAnnotated /= caseAnnotated meta ->
          ( OutcomeFail,
            "annotated:\nexpected:\n" <> unlines (caseAnnotated meta) <> "\nfound:\n" <> unlines actualAnnotated
          )
      | otherwise -> (OutcomePass, "")
    StatusFail ->
      ( OutcomeFail,
        "expected failure but resolver succeeded"
      )
    StatusXFail ->
      (OutcomeFail, "expected xfail (known failing bug), but resolver succeeded")
    StatusXPass
      | actual == caseExpected meta -> (OutcomeXPass, "known bug still passes unexpectedly")
      | otherwise ->
          ( OutcomeFail,
            "expected xpass output match but got output=" <> actual
          )

classifyFailure :: ResolverCase -> String -> (Outcome, String)
classifyFailure meta errDetails =
  case caseStatus meta of
    StatusPass ->
      ( OutcomeFail,
        "expected success, got error: " <> errDetails
      )
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass ->
      ( OutcomeFail,
        "expected xpass (known passing bug), got error: " <> errDetails
      )

progressSummary :: [(ResolverCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

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
    _ -> Left ("Invalid [status] in " <> path <> ": " <> T.unpack raw)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
        _ -> Right trimmed

validateExpected :: FilePath -> ExpectedStatus -> String -> Either String String
validateExpected path status expected =
  let trimmed = trim expected
   in case status of
        StatusPass | null trimmed -> Left ("[expected] is required for pass status in " <> path)
        StatusXPass | null trimmed -> Left ("[expected] is required for xpass status in " <> path)
        _ -> Right trimmed

validateAnnotated :: FilePath -> ExpectedStatus -> [String] -> Either String [String]
validateAnnotated path status annotated =
  case status of
    StatusPass | null annotated || all null annotated -> Left ("[annotated] is required for pass status in " <> path)
    StatusXPass | null annotated || all null annotated -> Left ("[annotated] is required for xpass status in " <> path)
    _ -> Right (map trim annotated)

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
