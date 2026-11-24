{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
    LanguageEdition (Haskell2010Edition),
    SourceSpan,
    fromAnnotation,
    parseExtensionName,
  )
import Aihc.Resolve
  ( Entity (..),
    GlobalName (..),
    Identifier,
    LocalId (..),
    ModuleUnit (..),
    Package (..),
    PackageId (..),
    ResolutionNamespace (..),
    ResolutionResult (..),
    ResolveError (..),
    ResolveFailure (..),
    Resolved (..),
    ResolvedModule (..),
    ResolvedUnit (..),
    builtins,
    collectModuleExports,
    displayIdentifier,
    resolveUnit,
    unnamedPackage,
  )
import qualified Aihc.Resolve.Syntax as R
import qualified Aihc.Resolve.Syntax.Traverse as RWalk
import Aihc.Testing.AnnotatedModule (renderLabeledSource)
import Aihc.Testing.Extensions (fixtureExtensions)
import Control.Applicative ((<|>))
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
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
    caseModules :: ![(Package, Text)],
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

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], [(Package, Text)], [Text], Text, Text)
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

parseModules :: Y.Value -> Y.Parser [(Package, Text)]
parseModules value = parseList value <|> parsePackages value
  where
    parseList = withArray "modules" $ \arr -> mapM (fmap (unnamedPackage,) . parseModuleEntry) (toList arr)
    parsePackages = withObject "package modules" $ \obj -> concat <$> mapM parsePackage (KeyMap.toList obj)
    parsePackage (packageIdKey, entry) = withObject "package modules" parsePackageEntry entry
      where
        parsePackageEntry obj = do
          visibleName <- obj .: "package"
          entries <- obj .: "modules"
          let package = Package visibleName (PackageId (Key.toText packageIdKey))
          withArray "package module list" (mapM (fmap (package,) . parseModuleEntry) . toList) entries
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
  let supportModules =
        [ (Package "aihc-prim" (PackageId "aihc-prim"), listSupportModule)
        | not (any (T.isPrefixOf "module GHC.Types" . T.stripStart . snd) (caseModules meta))
        ]
      supportModuleCount = length supportModules
      parsedModules = map (traverse parseOne) (supportModules <> caseModules meta)
   in case sequence parsedModules of
        Left errMsg -> (OutcomeFail, "parse error: " <> errMsg)
        Right parsed ->
          let modules = [ModuleUnit package (fixtureExtensions fixtureLanguageEdition ast) ast | (package, ast) <- parsed]
              exports = collectModuleExports modules
              builtinScope = builtins unnamedPackage exports builtinModuleNames
              resolved = either failureModules resolvedModules (resolveUnit builtinScope exports modules)
              fixtureModules = drop supportModuleCount resolved
              actualAnnotated = showAnnotated fixtureModules
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
    showAnnotated = renderAnnotatedResolveResult (map snd (caseModules meta))

-- | Fixtures have no cabal file. They compile under one language edition,
-- with whatever their own pragmas add to it.
fixtureLanguageEdition :: LanguageEdition
fixtureLanguageEdition = Haskell2010Edition

builtinModuleNames :: [Text]
builtinModuleNames = ["GHC.IsList", "GHC.Base", "GHC.Classes", "GHC.Num", "GHC.Prim", "GHC.Prim.String", "GHC.Real", "GHC.Records", "GHC.Types"]

listSupportModule :: Text
listSupportModule =
  T.unlines
    [ "module GHC.Types (List(..)) where",
      "data List a = [] | a : [a]",
      "infixr 5 :"
    ]

progressSummary :: [(ResolverCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

renderAnnotatedResolveResult :: [Text] -> [ResolvedModule] -> [String]
renderAnnotatedResolveResult sources resolved =
  case compare (length sources) (length modules) of
    LT -> error "renderAnnotatedResolveResult: fewer source texts than modules"
    GT -> error "renderAnnotatedResolveResult: more source texts than modules"
    EQ ->
      let moduleSources = sortOn (moduleDisplayName . fst) (zip modules sources)
       in [renderLabeledSource source (moduleLabels modu) | (modu, source) <- moduleSources]
  where
    modules = map resolvedModuleSyntax resolved

-- | The labels of a resolved module. A resolution goes at the span of the
-- syntax that it resolves:
--
-- * A name or a binder: the span of its annotations.
-- * A wrapper constructor, such as 'R.EResolved': the first span of the
--   chain of wrappers around the same node.
-- * An import declaration or a rule: the span of its annotations.
moduleLabels :: R.Module -> [(SourceSpan, Doc ann)]
moduleLabels modu =
  concatMap importLabels (R.moduleImports modu) <> RWalk.runCollect (RWalk.walk labelWalk) modu
  where
    importLabels decl = errorLabels (R.importDeclAnns decl) (R.importDeclError decl)

labelWalk :: RWalk.Walk (RWalk.Collect (SourceSpan, Doc ann))
labelWalk =
  RWalk.idWalk
    { RWalk.walkAnnotationList = \_ anns -> RWalk.collected (labelsAt (spanFromAnnotations anns) (mapMaybe resolutionAnnotationDoc anns)),
      RWalk.walkName = \children name ->
        RWalk.collected (resultLabels (R.nameAnns name) (R.nameResolution name)) *> children name,
      RWalk.walkUnqualifiedName = \children name ->
        RWalk.collected (resultLabels (R.unqualifiedNameAnns name) (R.unqualifiedNameResolution name)) *> children name,
      RWalk.walkExpr = wrapped peelExpr,
      RWalk.walkPattern = wrapped peelPattern,
      RWalk.walkType = wrapped peelType,
      RWalk.walkDecl = \children decl -> wrapped peelDecl (\base -> ruleLabels base *> children base) decl,
      RWalk.walkDataConDecl = wrapped peelDataCon,
      RWalk.walkLiteral = wrapped peelLiteral,
      RWalk.walkGuardQualifier = wrapped peelGuard,
      RWalk.walkDoStmt = wrapped peelDoStmt,
      RWalk.walkCompStmt = wrapped peelCompStmt,
      RWalk.walkArithSeq = wrapped peelArithSeq,
      RWalk.walkClassDeclItem = wrapped peelClassItem,
      RWalk.walkInstanceDeclItem = wrapped peelInstanceItem,
      RWalk.walkCmd = wrapped peelCmd,
      RWalk.walkExportSpec = wrapped peelExport,
      RWalk.walkImportItem = wrapped peelImportItem
    }
  where
    resultLabels anns result = labelsAt (spanFromAnnotations anns) (map resultDoc (maybeToList result))
    ruleLabels decl =
      case decl of
        R.DeclRules rules -> RWalk.collected (concat [errorLabels (R.ruleAnns rule) (R.ruleError rule) | rule <- rules])
        _ -> pure decl

-- | The labels of the error of an import declaration or a rule.
errorLabels :: [Annotation] -> Maybe ResolveError -> [(SourceSpan, Doc ann)]
errorLabels anns resolveError = labelsAt (spanFromAnnotations anns) (map (pretty . errorLabel) (maybeToList resolveError))

-- | One wrapper of a node: an annotation, or a result of the resolver.
data Wrapper
  = WrapperAnnotation Annotation
  | WrapperResult ResolutionResult
  | WrapperError ResolveError
  | WrapperOther

-- | Label the chain of wrappers around a node at the first span of the
-- chain. Then walk the children of the node below the chain.
wrapped :: (node -> Maybe (Wrapper, node)) -> (node -> RWalk.Collect (SourceSpan, Doc ann) node) -> node -> RWalk.Collect (SourceSpan, Doc ann) node
wrapped peel children node =
  case peelAll [] node of
    ([], _) -> children node
    (wrappers, base) ->
      let anns = [ann | WrapperAnnotation ann <- wrappers]
          docs = concatMap wrapperDoc wrappers
       in RWalk.collected (labelsAt (spanFromAnnotations anns) docs) *> (node <$ children base)
  where
    peelAll wrappers current =
      case peel current of
        Just (wrapper, inner) -> peelAll (wrapper : wrappers) inner
        Nothing -> (reverse wrappers, current)
    wrapperDoc wrapper =
      case wrapper of
        WrapperAnnotation ann -> maybeToList (resolutionAnnotationDoc ann)
        WrapperResult result -> [resultDoc result]
        WrapperError resolveError -> [pretty (errorLabel resolveError)]
        WrapperOther -> []

peelExpr :: R.Expr -> Maybe (Wrapper, R.Expr)
peelExpr expr =
  case expr of
    R.EAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.EResolved result inner -> Just (WrapperResult result, inner)
    _ -> Nothing

peelPattern :: R.Pattern -> Maybe (Wrapper, R.Pattern)
peelPattern pat =
  case pat of
    R.PAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.PResolved result inner -> Just (WrapperResult result, inner)
    _ -> Nothing

peelType :: R.Type -> Maybe (Wrapper, R.Type)
peelType ty =
  case ty of
    R.TAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.TResolved result inner -> Just (WrapperResult result, inner)
    _ -> Nothing

peelDecl :: R.Decl -> Maybe (Wrapper, R.Decl)
peelDecl decl =
  case decl of
    R.DeclAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.DeclError resolveError inner -> Just (WrapperError resolveError, inner)
    _ -> Nothing

peelDataCon :: R.DataConDecl -> Maybe (Wrapper, R.DataConDecl)
peelDataCon decl =
  case decl of
    R.DataConAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.DataConResolved result inner -> Just (WrapperResult result, inner)
    _ -> Nothing

peelLiteral :: R.Literal -> Maybe (Wrapper, R.Literal)
peelLiteral literal =
  case literal of
    R.LitAnn ann inner -> Just (WrapperAnnotation ann, inner)
    _ -> Nothing

peelGuard :: R.GuardQualifier -> Maybe (Wrapper, R.GuardQualifier)
peelGuard qualifier =
  case qualifier of
    R.GuardAnn ann inner -> Just (WrapperAnnotation ann, inner)
    _ -> Nothing

peelDoStmt :: R.DoStmt body -> Maybe (Wrapper, R.DoStmt body)
peelDoStmt stmt =
  case stmt of
    R.DoAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.DoResolved result inner -> Just (WrapperResult result, inner)
    R.DoRecKnot _ inner -> Just (WrapperOther, inner)
    _ -> Nothing

peelCompStmt :: R.CompStmt -> Maybe (Wrapper, R.CompStmt)
peelCompStmt stmt =
  case stmt of
    R.CompAnn ann inner -> Just (WrapperAnnotation ann, inner)
    _ -> Nothing

peelArithSeq :: R.ArithSeq -> Maybe (Wrapper, R.ArithSeq)
peelArithSeq arithSeq =
  case arithSeq of
    R.ArithSeqAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.ArithSeqResolved result inner -> Just (WrapperResult result, inner)
    _ -> Nothing

peelClassItem :: R.ClassDeclItem -> Maybe (Wrapper, R.ClassDeclItem)
peelClassItem item =
  case item of
    R.ClassItemAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.ClassItemError resolveError inner -> Just (WrapperError resolveError, inner)
    _ -> Nothing

peelInstanceItem :: R.InstanceDeclItem -> Maybe (Wrapper, R.InstanceDeclItem)
peelInstanceItem item =
  case item of
    R.InstanceItemAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.InstanceItemError resolveError inner -> Just (WrapperError resolveError, inner)
    _ -> Nothing

peelCmd :: R.Cmd -> Maybe (Wrapper, R.Cmd)
peelCmd cmd =
  case cmd of
    R.CmdAnn ann inner -> Just (WrapperAnnotation ann, inner)
    _ -> Nothing

peelExport :: R.ExportSpec -> Maybe (Wrapper, R.ExportSpec)
peelExport spec =
  case spec of
    R.ExportAnn ann inner -> Just (WrapperAnnotation ann, inner)
    _ -> Nothing

peelImportItem :: R.ImportItem -> Maybe (Wrapper, R.ImportItem)
peelImportItem item =
  case item of
    R.ImportAnn ann inner -> Just (WrapperAnnotation ann, inner)
    R.ImportItemError resolveError inner -> Just (WrapperError resolveError, inner)
    _ -> Nothing

-- | Put each label at a span. A label without a span is a fault of the
-- resolver.
labelsAt :: Maybe SourceSpan -> [Doc ann] -> [(SourceSpan, Doc ann)]
labelsAt _ [] = []
labelsAt (Just span') docs = map (span',) docs
labelsAt Nothing docs = error ("renderAnnotatedResolveResult: a label has no source span: " <> show (map show docs))

spanFromAnnotations :: [Annotation] -> Maybe SourceSpan
spanFromAnnotations = listToMaybe . mapMaybe fromAnnotation

resultDoc :: ResolutionResult -> Doc ann
resultDoc result =
  case result of
    ResolutionOk resolution -> pretty (annotationLabel resolution)
    ResolutionError resolveError -> pretty (errorLabel resolveError)

-- | The label of a resolution that succeeded, or of one that failed.
resolutionAnnotationDoc :: Annotation -> Maybe (Doc ann)
resolutionAnnotationDoc annotation =
  (pretty . annotationLabel <$> fromAnnotation annotation)
    <|> (pretty . errorLabel <$> fromAnnotation annotation)

errorLabel :: ResolveError -> Text
errorLabel resolveError =
  renderConciseNamespace (resolveErrorNamespace resolveError) <> " Error " <> T.pack (resolveErrorMessage resolveError)

moduleDisplayName :: R.Module -> Text
moduleDisplayName modu = fromMaybe (T.pack "<unnamed>") (R.moduleName modu)

annotationLabel :: Resolved -> Text
annotationLabel ann =
  renderConciseNamespace (resolutionNamespace ann)
    <> " "
    <> renderConciseOrigin (resolutionIdentifier ann) (resolutionTarget ann)

renderConciseNamespace :: ResolutionNamespace -> Text
renderConciseNamespace namespace =
  case namespace of
    ResolutionNamespaceTerm -> "v"
    ResolutionNamespaceType -> "t"
    ResolutionNamespaceModule -> "m"

renderConciseOrigin :: Identifier -> Entity -> Text
renderConciseOrigin identifier entity =
  case entity of
    EntityGlobal global
      | packageIdText (globalNamePackage global) `elem` ["", "main"] -> globalNameModule global
      | otherwise -> packageIdText (globalNamePackage global) <> ":" <> globalNameModule global
    EntityLocal (LocalId uniqueId) -> T.pack (show uniqueId)
    EntitySyntax -> "Builtin " <> displayIdentifier identifier

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
