{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import Aihc.Parser.Syntax
  ( Annotation,
    ClassDecl (..),
    ClassDeclItem (..),
    Decl (..),
    Expr (..),
    Extension,
    InstanceDecl (..),
    InstanceDeclItem (..),
    Module (..),
    Pattern (..),
    SourceSpan (..),
    Type (..),
    fromAnnotation,
    moduleName,
    parseExtensionName,
  )
import Aihc.Tc (TcBindingResult (..), TcModuleResult (..), renderTcType, typecheck)
import Aihc.Tc.Annotations (TcAnnotation (..), TcClassAnnotation (..), TcInstanceAnnotation (..), TcInstanceMethodAnnotation (..))
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar (..))
import Aihc.Tc.Types (Pred (..), TcType, TyCon (..), Unique (..))
import Control.Applicative ((<|>))
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.Data (Data, cast, gmapQ)
import Data.List (dropWhileEnd, intercalate, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
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

data TcLocatedAnnotation = TcLocatedAnnotation
  { locatedSpan :: !SourceSpan,
    locatedKind :: !String,
    locatedLabel :: !String
  }
  deriving (Eq, Show)

renderTcAnnotations :: [Module] -> String
renderTcAnnotations modules =
  intercalate "\n" (map renderModule (sortOn fst (map collectModuleTcAnnotations modules)))
  where
    renderModule (moduleNameText, annotations) =
      T.unpack moduleNameText
        <> ":\n"
        <> intercalate "\n" (map (("  " <>) . renderLocatedAnnotation) (sortOn annotationKey (filter hasLocatedLabel annotations)))

renderAnnotatedTcModules :: [Text] -> [Module] -> [String]
renderAnnotatedTcModules sources modules =
  let moduleSourcePairs = zipWith (\source modu -> (moduleDisplayName modu, source)) sources modules
      annotationMap = Map.fromList (map collectModuleTcAnnotations modules)
   in map
        ( \(name, source) ->
            renderAnnotatedTcSource source (Map.findWithDefault [] name annotationMap)
        )
        (sortOn fst moduleSourcePairs)

collectModuleTcAnnotations :: Module -> (Text, [TcLocatedAnnotation])
collectModuleTcAnnotations modu =
  (moduleDisplayName modu, collectTcAnnotations NoSourceSpan modu)

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe (T.pack "<unnamed>") (moduleName modu)

collectTcAnnotations :: (Data a) => SourceSpan -> a -> [TcLocatedAnnotation]
collectTcAnnotations ambient node =
  fromMaybe (concat (gmapQ (collectTcAnnotations ambient) node)) $
    collectDecl <$> cast node
      <|> collectExpr <$> cast node
      <|> collectPattern <$> cast node
      <|> collectType <$> cast node
      <|> collectClassItem <$> cast node
      <|> collectInstanceItem <$> cast node
  where
    collectDecl (DeclAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationDeclTcItems ambient' inner ann <> collectTcAnnotations ambient' inner
    collectDecl other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectExpr (EAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "expr" ambient' ann <> collectTcAnnotations ambient' inner
    collectExpr other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectPattern (PAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "pattern" ambient' ann <> collectTcAnnotations ambient' inner
    collectPattern other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectType (TAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "type" ambient' ann <> collectTcAnnotations ambient' inner
    collectType other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectClassItem (ClassItemAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "class-item" ambient' ann <> collectTcAnnotations ambient' inner
    collectClassItem other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectInstanceItem (InstanceItemAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "instance-item" ambient' ann <> collectTcAnnotations ambient' inner
    collectInstanceItem other = concat (gmapQ (collectTcAnnotations ambient) other)

annotationSourceSpan :: SourceSpan -> Annotation -> SourceSpan
annotationSourceSpan ambient ann =
  fromMaybe ambient (fromAnnotation @SourceSpan ann)

annotationTcItems :: String -> SourceSpan -> Annotation -> [TcLocatedAnnotation]
annotationTcItems kind span' ann =
  concat
    [ map (locatedTcAnnotation kind span') (maybeToListAnn @TcAnnotation ann),
      map (locatedTcClassAnnotation span') (maybeToListAnn @TcClassAnnotation ann),
      map (locatedTcInstanceAnnotation span') (maybeToListAnn @TcInstanceAnnotation ann),
      map (locatedTcInstanceMethodAnnotation span') (maybeToListAnn @TcInstanceMethodAnnotation ann)
    ]

annotationDeclTcItems :: SourceSpan -> Decl -> Annotation -> [TcLocatedAnnotation]
annotationDeclTcItems span' inner ann =
  concat
    [ map (locatedTcAnnotation "decl" span') (maybeToListAnn @TcAnnotation ann),
      map (locatedTcClassAnnotation (refineDeclSpan span' inner)) (maybeToListAnn @TcClassAnnotation ann),
      map (locatedTcInstanceAnnotation (refineDeclSpan span' inner)) (maybeToListAnn @TcInstanceAnnotation ann),
      map (locatedTcInstanceMethodAnnotation span') (maybeToListAnn @TcInstanceMethodAnnotation ann)
    ]

refineDeclSpan :: SourceSpan -> Decl -> SourceSpan
refineDeclSpan span' decl =
  case decl of
    DeclClass classDecl -> spanThroughItems span' (concatMap classItemSourceSpans (classDeclItems classDecl))
    DeclInstance instanceDecl -> spanThroughItems span' (concatMap instanceItemSourceSpans (instanceDeclItems instanceDecl))
    _ -> span'

classItemSourceSpans :: ClassDeclItem -> [SourceSpan]
classItemSourceSpans item =
  case item of
    ClassItemAnn ann inner -> annotationSourceSpan NoSourceSpan ann : classItemSourceSpans inner
    _ -> []

instanceItemSourceSpans :: InstanceDeclItem -> [SourceSpan]
instanceItemSourceSpans item =
  case item of
    InstanceItemAnn ann inner -> annotationSourceSpan NoSourceSpan ann : instanceItemSourceSpans inner
    _ -> []

spanThroughItems :: SourceSpan -> [SourceSpan] -> SourceSpan
spanThroughItems span' itemSpans =
  case latestSourceSpan itemSpans of
    Nothing -> span'
    Just itemSpan -> replaceSpanEnd span' itemSpan

latestSourceSpan :: [SourceSpan] -> Maybe SourceSpan
latestSourceSpan spans =
  case filter hasSourceSpan spans of
    [] -> Nothing
    realSpans -> Just (maximumByEnd realSpans)

maximumByEnd :: [SourceSpan] -> SourceSpan
maximumByEnd = foldr1 maxEnd
  where
    maxEnd left right
      | sourceSpanEndKey left >= sourceSpanEndKey right = left
      | otherwise = right

sourceSpanEndKey :: SourceSpan -> (Int, Int)
sourceSpanEndKey span' =
  case span' of
    SourceSpan _ _ _ endLine endCol _ _ -> (endLine, endCol)
    NoSourceSpan -> (minBound, minBound)

hasSourceSpan :: SourceSpan -> Bool
hasSourceSpan SourceSpan {} = True
hasSourceSpan NoSourceSpan = False

replaceSpanEnd :: SourceSpan -> SourceSpan -> SourceSpan
replaceSpanEnd startSpan endSpan =
  case (startSpan, endSpan) of
    (SourceSpan source startLine startCol _ _ startOffset _, SourceSpan _ _ _ endLine endCol _ endOffset) ->
      SourceSpan source startLine startCol endLine endCol startOffset endOffset
    _ -> startSpan

maybeToListAnn :: forall a. (Typeable a) => Annotation -> [a]
maybeToListAnn ann = maybe [] pure (fromAnnotation @a ann)

locatedTcAnnotation :: String -> SourceSpan -> TcAnnotation -> TcLocatedAnnotation
locatedTcAnnotation kind span' ann =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = kind,
      locatedLabel = renderTcAnnotationLabel ann
    }

locatedTcClassAnnotation :: SourceSpan -> TcClassAnnotation -> TcLocatedAnnotation
locatedTcClassAnnotation span' _ =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = "class",
      locatedLabel = "class"
    }

locatedTcInstanceAnnotation :: SourceSpan -> TcInstanceAnnotation -> TcLocatedAnnotation
locatedTcInstanceAnnotation span' ann =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = "instance",
      locatedLabel = T.unpack (tcInstanceDictName ann)
    }

locatedTcInstanceMethodAnnotation :: SourceSpan -> TcInstanceMethodAnnotation -> TcLocatedAnnotation
locatedTcInstanceMethodAnnotation span' ann =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = "instance-method",
      locatedLabel = T.unpack (tcInstanceMethodName ann)
    }

hasLocatedLabel :: TcLocatedAnnotation -> Bool
hasLocatedLabel = not . null . locatedLabel

renderLocatedAnnotation :: TcLocatedAnnotation -> String
renderLocatedAnnotation ann =
  renderSourceSpan (locatedSpan ann)
    <> " "
    <> locatedKind ann
    <> " => "
    <> locatedLabel ann

annotationKey :: TcLocatedAnnotation -> (Int, Int, Int, Int, String, String)
annotationKey ann =
  case locatedSpan ann of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      (startLine, startCol, endLine, endCol, locatedKind ann, locatedLabel ann)
    NoSourceSpan ->
      (maxBound, maxBound, maxBound, maxBound, locatedKind ann, locatedLabel ann)

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

renderTcAnnotationLabel :: TcAnnotation -> String
renderTcAnnotationLabel ann =
  unwords $
    map renderTypeApplication (tcAnnTypeArgs ann)
      <> map renderEvTerm (tcAnnEvidenceTerms ann)

renderTypeApplication :: TcType -> String
renderTypeApplication ty = "@" <> renderTcType ty

renderEvTerm :: EvTerm -> String
renderEvTerm ev =
  case ev of
    EvVarTerm var -> renderEvVar var
    EvGiven pred' -> "given(" <> renderPred pred' <> ")"
    EvDict name typeArgs evidence ->
      "dict "
        <> T.unpack name
        <> renderListField " typeArgs" renderTcType typeArgs
        <> renderListField " evidence" renderEvTerm evidence
    EvCoercion coercion -> "coercion(" <> renderCoercion coercion <> ")"
    EvSuperClass inner index -> "super[" <> show index <> "](" <> renderEvTerm inner <> ")"
    EvCast inner coercion -> "cast(" <> renderEvTerm inner <> ", " <> renderCoercion coercion <> ")"

renderEvVar :: EvVar -> String
renderEvVar (EvVar (Unique unique)) = "$ev" <> show unique

renderCoercion :: Coercion -> String
renderCoercion coercion =
  case coercion of
    CoVar var -> renderEvVar var
    Refl ty -> "refl " <> renderTcType ty
    Sym inner -> "sym(" <> renderCoercion inner <> ")"
    Trans left right -> "trans(" <> renderCoercion left <> ", " <> renderCoercion right <> ")"
    TyConAppCo tc coercions -> T.unpack (tyConName tc) <> renderListField " coercions" renderCoercion coercions
    AxiomInstCo name types -> "axiom " <> T.unpack name <> renderListField " types" renderTcType types

renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred cls args -> T.unpack cls <> " " <> unwords (map renderTcType args)
    EqPred left right -> renderTcType left <> " ~ " <> renderTcType right

renderListField :: String -> (a -> String) -> [a] -> String
renderListField _ _ [] = ""
renderListField label renderItem xs = label <> "=" <> renderList renderItem xs

renderList :: (a -> String) -> [a] -> String
renderList renderItem xs = "[" <> intercalate ", " (map renderItem xs) <> "]"

renderAnnotatedTcSource :: Text -> [TcLocatedAnnotation] -> String
renderAnnotatedTcSource source annotations =
  let sourceLines = T.lines source
      sorted = sortOn annotationKey annotations
      grouped = groupByLine sorted
   in intercalate "\n" (concatMap (renderSourceLine grouped) (zip [1 ..] sourceLines))

groupByLine :: [TcLocatedAnnotation] -> Map.Map Int [TcLocatedAnnotation]
groupByLine = foldr insertAnn Map.empty
  where
    insertAnn ann acc =
      case locatedSpan ann of
        SourceSpan _ startLine _ _ _ _ _ ->
          Map.insertWith (<>) startLine [ann] acc
        NoSourceSpan -> acc

renderSourceLine :: Map.Map Int [TcLocatedAnnotation] -> (Int, Text) -> [String]
renderSourceLine grouped (lineNum, sourceLine) =
  let annotations = maybe [] (sortOn annotationStartCol) (Map.lookup lineNum grouped)
   in T.unpack sourceLine : renderAnnotationLines annotations

annotationStartCol :: TcLocatedAnnotation -> Int
annotationStartCol ann =
  case locatedSpan ann of
    SourceSpan _ _ startCol _ _ _ _ -> startCol
    NoSourceSpan -> maxBound

renderAnnotationLines :: [TcLocatedAnnotation] -> [String]
renderAnnotationLines [] = []
renderAnnotationLines annotations =
  let items = [(annotationStartCol ann - 1, locatedLabel ann) | ann <- annotations, hasLocatedLabel ann]
   in layoutAnnotationLines items

type AnnotationItem = (Int, String)

layoutAnnotationLines :: [AnnotationItem] -> [String]
layoutAnnotationLines [] = []
layoutAnnotationLines items =
  let reversed = sortOn (Down . fst) items
      (currentLine, deferred) = packLine reversed
   in renderAnnotationLine currentLine deferred : layoutAnnotationLines deferred

packLine :: [AnnotationItem] -> ([AnnotationItem], [AnnotationItem])
packLine [] = ([], [])
packLine (rightmost : rest) =
  let go _ [] fitted deferred = (sortOn fst fitted, sortOn fst deferred)
      go minCol (item@(col, label) : remaining) fitted deferred =
        let annotEnd = col + 3 + length label
            fitsBeforePlaced = annotEnd < minCol
            crossesDeferred = any ((\d -> d > col && d < annotEnd) . fst) deferred
         in if fitsBeforePlaced && not crossesDeferred
              then go col remaining (item : fitted) deferred
              else go minCol remaining fitted (item : deferred)
   in go (fst rightmost) rest [rightmost] []

renderAnnotationLine :: [AnnotationItem] -> [AnnotationItem] -> String
renderAnnotationLine placedOnLine deferredItems =
  let deferredCols = map fst deferredItems
      buildLine _ [] [] = ""
      buildLine pos placed deferred =
        case (placed, deferred) of
          ((col, label) : restPlaced, _)
            | pos == col ->
                "\x2514\x2500 " <> label <> buildLine (pos + 3 + length label) restPlaced (filter (>= pos + 3 + length label) deferred)
          (_, d : restDeferred)
            | pos == d ->
                "\x2502" <> buildLine (pos + 1) placed restDeferred
          _ ->
            let nextPos = case (placed, deferred) of
                  ((col, _) : _, d : _) -> min col d
                  ((col, _) : _, []) -> col
                  ([], d : _) -> d
                padding = nextPos - pos
             in replicate padding ' ' <> buildLine nextPos placed deferred
   in buildLine 0 (sortOn fst placedOnLine) (sortOn id deferredCols)

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
