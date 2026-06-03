{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ResolverGolden
  ( ExpectedStatus (..),
    Outcome (..),
    ResolverCase (..),
    fixtureRoot,
    loadResolverCases,
    parseResolverCaseText,
    evaluateResolverCase,
    progressSummary,
    renderResolveResult,
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
    ClassDeclItem (..),
    Decl,
    Expr,
    Extension,
    ImportDecl (..),
    Module,
    Name (..),
    Pattern,
    SourceSpan (..),
    Type,
    UnqualifiedName (..),
    fromAnnotation,
    moduleName,
    parseExtensionName,
    renderName,
    renderUnqualifiedName,
  )
import Aihc.Resolve
  ( ResolutionAnnotation (..),
    ResolutionNamespace (..),
    ResolveResult (..),
    ResolvedName (..),
    resolve,
    pattern DeclResolution,
    pattern EResolution,
    pattern PResolution,
    pattern TResolution,
  )
import Aihc.Testing.AnnotatedModule (renderAnnotatedModules)
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.Data (Data, cast, gmapQ)
import Data.List (dropWhileEnd, intercalate, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
    parseExpectedModule (expectedModuleName, moduleValue) = do
      moduleLines <- parseExpectedModuleLines moduleValue
      pure (Key.toText expectedModuleName <> ":\n" <> T.intercalate "\n" (map ("  " <>) moduleLines))
    parseExpectedModuleLines (Y.String txt) = pure [txt]
    parseExpectedModuleLines (Y.Array arr) = mapM parseExpectedLine (toList arr)
    parseExpectedModuleLines _ = fail "each expected module value must be a string or a list of strings"
    parseExpectedLine (Y.String txt) = pure txt
    parseExpectedLine _ = fail "each expected list entry must be a string"

evaluateResolverCase :: ResolverCase -> (Outcome, String)
evaluateResolverCase meta =
  let parsedModules = map parseOne (caseModules meta)
   in case sequence parsedModules of
        Left errMsg -> (OutcomeFail, "parse error: " <> errMsg)
        Right modules ->
          let result = resolve modules
              actual = showResolved result
              actualAnnotated = showAnnotated result
              outputMatches = actual == caseExpected meta && (null (caseAnnotated meta) || actualAnnotated == caseAnnotated meta)
           in case caseStatus meta of
                StatusPass
                  | actual /= caseExpected meta ->
                      ( OutcomeFail,
                        "expected:\n" <> caseExpected meta <> "\nfound:\n" <> actual
                      )
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
                  | otherwise -> (OutcomeFail, "expected xpass output match but got output=" <> actual)
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

renderResolveResult :: ResolveResult -> String
renderResolveResult result =
  intercalate "\n" (map renderModuleAnnotations (sortOn fst (mergeModules (collectModules (resolvedModules result) <> resolvedAnnotations result))))

renderAnnotatedResolveResult :: ParserConfig -> ResolveResult -> [String]
renderAnnotatedResolveResult parserConfig result =
  renderAnnotatedModules parserConfig resolutionAnnotationDoc (sortOn moduleDisplayName (resolvedModules result))

resolutionAnnotationDoc :: Annotation -> Maybe (Doc ann)
resolutionAnnotationDoc annotation = do
  resolution <- fromAnnotation annotation
  pure (pretty (annotationLabel resolution))

renderResolvedName :: ResolvedName -> String
renderResolvedName resolvedName =
  case resolvedName of
    ResolvedTopLevel name -> T.unpack (renderName name)
    ResolvedLocal uniqueId localName -> "Local " <> show uniqueId <> " " <> T.unpack (renderUnqualifiedName localName)
    ResolvedBuiltin name -> "Builtin " <> T.unpack name
    ResolvedError msg -> "Error " <> msg

renderResolutionAnnotation :: ResolutionAnnotation -> String
renderResolutionAnnotation ann =
  renderSourceSpan (resolutionSpan ann)
    <> " "
    <> T.unpack (resolutionName ann)
    <> " => "
    <> renderResolutionNamespace (resolutionNamespace ann)
    <> " "
    <> renderResolvedName (resolutionTarget ann)

renderResolutionNamespace :: ResolutionNamespace -> String
renderResolutionNamespace namespace =
  case namespace of
    ResolutionNamespaceTerm -> "(value)"
    ResolutionNamespaceType -> "(type)"
    ResolutionNamespaceModule -> "(module)"

annotationKey :: ResolutionAnnotation -> (Int, Int, Int, Int, Text)
annotationKey ann =
  case resolutionSpan ann of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      (startLine, startCol, endLine, endCol, resolutionName ann)
    NoSourceSpan ->
      (maxBound, maxBound, maxBound, maxBound, resolutionName ann)

renderSourceSpan :: SourceSpan -> String
renderSourceSpan span' =
  case span' of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      show startLine
        <> ":"
        <> show startCol
        <> "-"
        <> show endLine
        <> ":"
        <> show endCol
    NoSourceSpan -> "<no-source>"

collectModules :: [Module] -> [(Text, [ResolutionAnnotation])]
collectModules = map collectModuleAnnotations

mergeModules :: [(Text, [ResolutionAnnotation])] -> [(Text, [ResolutionAnnotation])]
mergeModules modules =
  Map.toList (Map.fromListWith (<>) modules)

collectModuleAnnotations :: Module -> (Text, [ResolutionAnnotation])
collectModuleAnnotations modu =
  (moduleDisplayName modu, collectAnnotations modu)

collectAnnotations :: (Data a) => a -> [ResolutionAnnotation]
collectAnnotations node =
  ownAnnotation node <> concat (gmapQ collectAnnotations node)

ownAnnotation :: (Data a) => a -> [ResolutionAnnotation]
ownAnnotation node =
  declResolution (cast node)
    <> classDeclItemResolution (cast node)
    <> importResolution (cast node)
    <> nameResolution (cast node)
    <> unqualifiedNameResolution (cast node)
    <> patternResolution (cast node)
    <> typeResolution (cast node)
    <> exprResolution (cast node)

declResolution :: Maybe Decl -> [ResolutionAnnotation]
declResolution maybeDecl =
  case maybeDecl of
    Just (DeclResolution resolution) -> [resolution]
    _ -> []

classDeclItemResolution :: Maybe ClassDeclItem -> [ResolutionAnnotation]
classDeclItemResolution maybeItem =
  case maybeItem of
    Just (ClassItemAnn (fromAnnotation -> Just resolution) _) -> [resolution]
    _ -> []

importResolution :: Maybe ImportDecl -> [ResolutionAnnotation]
importResolution maybeImport =
  case maybeImport of
    Just importDecl -> importResolutionAnnotations importDecl
    _ -> []

importResolutionAnnotations :: ImportDecl -> [ResolutionAnnotation]
importResolutionAnnotations = mapMaybe fromAnnotation . importDeclAnns

nameResolution :: Maybe Name -> [ResolutionAnnotation]
nameResolution maybeName =
  case maybeName of
    Just name -> mapMaybe fromAnnotation (nameAnns name)
    _ -> []

unqualifiedNameResolution :: Maybe UnqualifiedName -> [ResolutionAnnotation]
unqualifiedNameResolution maybeName =
  case maybeName of
    Just name -> mapMaybe fromAnnotation (unqualifiedNameAnns name)
    _ -> []

patternResolution :: Maybe Pattern -> [ResolutionAnnotation]
patternResolution maybePattern =
  case maybePattern of
    Just (PResolution resolution) -> [resolution]
    _ -> []

typeResolution :: Maybe Type -> [ResolutionAnnotation]
typeResolution maybeType =
  case maybeType of
    Just (TResolution resolution) -> [resolution]
    _ -> []

exprResolution :: Maybe Expr -> [ResolutionAnnotation]
exprResolution maybeExpr =
  case maybeExpr of
    Just (EResolution resolution) -> [resolution]
    _ -> []

renderModuleAnnotations :: (Text, [ResolutionAnnotation]) -> String
renderModuleAnnotations (moduleNameText, annotations) =
  T.unpack moduleNameText
    <> ":\n"
    <> intercalate "\n" (map (("  " <>) . renderResolutionAnnotation) (sortOn annotationKey annotations))

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
