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
    renderAnnotatedResolveResult,
  )
where

import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    formatParseErrors,
    parseModule,
  )
import Aihc.Parser.Syntax
  ( Annotation,
    Extension,
    Module,
    Name (..),
    fromAnnotation,
    moduleName,
    parseExtensionName,
    renderName,
  )
import Aihc.Resolve
  ( ResolutionAnnotation (..),
    ResolutionNamespace (..),
    ResolveResult (..),
    ResolvedName (..),
    resolve,
  )
import Aihc.Testing.AnnotatedModule (renderAnnotatedModules)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Prettyprinter (Doc, pretty)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

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

data ResolverCase = ResolverCase
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
  (extNames, modules, annotatedTexts, statusText, reasonText) <- parseYamlFixture path value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
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
        caseAnnotated = annotated,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], [Text], [Text], Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "resolver fixture" $ \obj -> do
        exts <- obj .: "extensions"
        modules <- obj .: "modules" >>= parseModules
        annotatedTexts <- obj .: "annotated" >>= parseAnnotatedList
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, modules, annotatedTexts, statusText, reasonText)
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

evaluateResolverCase :: ResolverCase -> (Outcome, String)
evaluateResolverCase meta =
  let parsedModules = map parseOne (caseModules meta)
   in case sequence parsedModules of
        Left errMsg -> (OutcomeFail, "parse error: " <> errMsg)
        Right modules ->
          let result = resolve modules
              actualAnnotated = showAnnotated result
              outputMatches = actualAnnotated == caseAnnotated meta
           in case caseStatus meta of
                StatusPass
                  | actualAnnotated /= caseAnnotated meta ->
                      ( OutcomeFail,
                        "annotated:\nexpected:\n" <> unlines (caseAnnotated meta) <> "\nfound:\n" <> unlines actualAnnotated
                      )
                  | otherwise -> (OutcomePass, "")
                StatusXFail
                  | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
                  | otherwise -> (OutcomeXFail, "")
                StatusXPass
                  | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
                  | otherwise -> (OutcomeFail, "expected xpass annotated output match")
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
    showAnnotated =
      renderAnnotatedResolveResult
        ( defaultConfig
            { parserSourceName = "<resolver-annotated>",
              parserExtensions = caseExtensions meta
            }
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

renderAnnotatedResolveResult :: ParserConfig -> ResolveResult -> [String]
renderAnnotatedResolveResult parserConfig result =
  renderAnnotatedModules parserConfig resolutionAnnotationDoc (sortOn moduleDisplayName (resolvedModules result))

resolutionAnnotationDoc :: Annotation -> Maybe (Doc ann)
resolutionAnnotationDoc annotation = do
  resolution <- fromAnnotation annotation
  pure (pretty (annotationLabel resolution))

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe (T.pack "<unnamed>") (moduleName modu)

annotationLabel :: ResolutionAnnotation -> Text
annotationLabel ann =
  renderConciseNamespace (resolutionNamespace ann)
    <> " "
    <> renderConciseOrigin (resolutionTarget ann)

renderConciseNamespace :: ResolutionNamespace -> Text
renderConciseNamespace namespace =
  case namespace of
    ResolutionNamespaceTerm -> "v"
    ResolutionNamespaceType -> "t"
    ResolutionNamespaceModule -> "m"

renderConciseOrigin :: ResolvedName -> Text
renderConciseOrigin resolvedName =
  case resolvedName of
    ResolvedTopLevel name -> fromMaybe (renderName name) (nameQualifier name)
    ResolvedLocal uniqueId _ -> T.pack (show uniqueId)
    ResolvedBuiltin name -> "Builtin " <> name
    ResolvedError msg -> T.pack ("Error " <> msg)

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
    _ -> Left ("Invalid [status] in " <> path <> ": " <> T.unpack raw)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
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
