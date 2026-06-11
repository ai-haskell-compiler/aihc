{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Aihc.Testing.AnnotatedModule
  ( AnnotationLabel (..),
    renderAnnotatedModule,
    renderAnnotatedModules,
    renderAnnotatedModuleSource,
    renderAnnotatedModuleSources,
    renderAnnotatedModuleWithLabels,
    renderAnnotatedModulesWithLabels,
    renderAnnotatedModuleSourceWithLabels,
    renderAnnotatedModuleSourcesWithLabels,
  )
where

import Aihc.Parser (ParserConfig, formatParseErrors, parseModule)
import Aihc.Parser.Parens (addModuleParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    DoStmt (..),
    ExportSpec (..),
    Expr (..),
    GuardQualifier (..),
    ImportItem (..),
    InstanceDeclItem (..),
    Literal (..),
    Module,
    Pattern (..),
    SourceSpan (..),
    Type (..),
    fromAnnotation,
    stripAnnotations,
  )
import Control.Applicative ((<|>))
import Data.Data (Data, cast, gmapQ, toConstr)
import Data.List (intercalate, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)

data AnnotationLabel ann = AnnotationLabel
  { annotationLabelSpan :: !SourceSpan,
    annotationLabelDoc :: !(Doc ann),
    annotationLabelGroup :: !(Maybe String)
  }

type AnnotationRenderer ann =
  forall carrier.
  (Data carrier) =>
  SourceSpan ->
  Maybe SourceSpan ->
  carrier ->
  Annotation ->
  [AnnotationLabel ann]

renderAnnotatedModule :: ParserConfig -> (Annotation -> Maybe (Doc ann)) -> Module -> String
renderAnnotatedModule parserConfig renderAnnotation =
  renderAnnotatedModuleWithLabels parserConfig (defaultAnnotationLabels renderAnnotation)

renderAnnotatedModules :: ParserConfig -> (Annotation -> Maybe (Doc ann)) -> [Module] -> [String]
renderAnnotatedModules parserConfig renderAnnotation =
  renderAnnotatedModulesWithLabels parserConfig (defaultAnnotationLabels renderAnnotation)

renderAnnotatedModuleSource :: (Annotation -> Maybe (Doc ann)) -> T.Text -> Module -> String
renderAnnotatedModuleSource renderAnnotation =
  renderAnnotatedModuleSourceWithLabels (defaultAnnotationLabels renderAnnotation)

renderAnnotatedModuleSources :: (Annotation -> Maybe (Doc ann)) -> [T.Text] -> [Module] -> [String]
renderAnnotatedModuleSources renderAnnotation =
  renderAnnotatedModuleSourcesWithLabels (defaultAnnotationLabels renderAnnotation)

defaultAnnotationLabels :: (Annotation -> Maybe (Doc ann)) -> SourceSpan -> Maybe SourceSpan -> carrier -> Annotation -> [AnnotationLabel ann]
defaultAnnotationLabels renderAnnotation span' _ _ annotation =
  [AnnotationLabel span' doc Nothing | Just doc <- [renderAnnotation annotation]]

renderAnnotatedModuleWithLabels ::
  ParserConfig ->
  AnnotationRenderer ann ->
  Module ->
  String
renderAnnotatedModuleWithLabels parserConfig renderAnnotation modu =
  length labels `seq` renderAnnotatedSource prettySource labels
  where
    prettyModule = addModuleParens modu
    prettySource = renderDoc (pretty prettyModule)
    (parseErrors, reparsed) = parseModule parserConfig (T.pack prettySource)
    labels
      | not (null parseErrors) =
          error ("renderAnnotatedModule: pretty-printed module did not parse:\n" <> formatParseErrors "<pretty>" (Just (T.pack prettySource)) parseErrors)
      | stripAnnotations prettyModule /= stripAnnotations reparsed =
          error
            ( "renderAnnotatedModule: pretty-printed module changed AST shape\noriginal:\n"
                <> show (stripAnnotations prettyModule)
                <> "\nreparsed:\n"
                <> show (stripAnnotations reparsed)
            )
      | otherwise = collectLabels renderAnnotation prettyModule reparsed

renderAnnotatedModulesWithLabels ::
  ParserConfig ->
  AnnotationRenderer ann ->
  [Module] ->
  [String]
renderAnnotatedModulesWithLabels parserConfig renderAnnotation =
  map (renderAnnotatedModuleWithLabels parserConfig renderAnnotation)

renderAnnotatedModuleSourceWithLabels ::
  AnnotationRenderer ann ->
  T.Text ->
  Module ->
  String
renderAnnotatedModuleSourceWithLabels renderAnnotation source modu =
  length labels `seq` renderAnnotatedSource (T.unpack source) labels
  where
    labels = collectLabels renderAnnotation modu modu

renderAnnotatedModuleSourcesWithLabels ::
  AnnotationRenderer ann ->
  [T.Text] ->
  [Module] ->
  [String]
renderAnnotatedModuleSourcesWithLabels renderAnnotation sources modules =
  case compare (length sources) (length modules) of
    LT -> error "renderAnnotatedModuleSources: fewer source texts than modules"
    GT -> error "renderAnnotatedModuleSources: more source texts than modules"
    EQ -> zipWith (renderAnnotatedModuleSourceWithLabels renderAnnotation) sources modules

data PlacedLabel = PlacedLabel
  { placedSpan :: !SourceSpan,
    placedText :: !String,
    placedGroup :: !(Maybe String)
  }
  deriving (Eq, Show)

data SomeData = forall a. (Data a) => SomeData a

collectLabels :: AnnotationRenderer ann -> Module -> Module -> [PlacedLabel]
collectLabels renderAnnotation =
  collectPair renderAnnotation Nothing

collectPair :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> [PlacedLabel]
collectPair renderAnnotation ambient original reparsed =
  fromMaybe (collectGeneric renderAnnotation ambient original reparsed) (collectSpecial renderAnnotation ambient original reparsed)

collectSpecial :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectSpecial renderAnnotation ambient original reparsed =
  collectAnnotationList renderAnnotation ambient original reparsed
    <|> collectAnnotation renderAnnotation ambient original reparsed
    <|> collectExpr renderAnnotation ambient original reparsed
    <|> collectType renderAnnotation ambient original reparsed
    <|> collectPattern renderAnnotation ambient original reparsed
    <|> collectDecl renderAnnotation ambient original reparsed
    <|> collectDataConDecl renderAnnotation ambient original reparsed
    <|> collectLiteral renderAnnotation ambient original reparsed
    <|> collectGuardQualifier renderAnnotation ambient original reparsed
    <|> collectDoStmtExpr renderAnnotation ambient original reparsed
    <|> collectDoStmtCmd renderAnnotation ambient original reparsed
    <|> collectCompStmt renderAnnotation ambient original reparsed
    <|> collectArithSeq renderAnnotation ambient original reparsed
    <|> collectClassDeclItem renderAnnotation ambient original reparsed
    <|> collectInstanceDeclItem renderAnnotation ambient original reparsed
    <|> collectCmd renderAnnotation ambient original reparsed
    <|> collectExportSpec renderAnnotation ambient original reparsed
    <|> collectImportItem renderAnnotation ambient original reparsed

collectAnnotationList :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectAnnotationList renderAnnotation ambient original reparsed = do
  originalAnns <- cast original
  reparsedAnns <- cast reparsed
  pure (labelsAt renderAnnotation (spanFromAnnotations reparsedAnns) ambient originalAnns originalAnns)

collectAnnotation :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectAnnotation renderAnnotation ambient original reparsed = do
  originalAnn <- cast original
  reparsedAnn <- cast reparsed
  pure (labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) ambient originalAnn [originalAnn])

collectExpr :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectExpr renderAnnotation ambient original reparsed = do
  originalExpr <- cast original
  reparsedExpr <- cast reparsed
  pure $
    case (originalExpr, reparsedExpr) of
      (EAnn originalAnn originalInner, EAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (EAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedExpr) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedExpr
      (_, EAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalExpr reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalExpr reparsedExpr

collectType :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectType renderAnnotation ambient original reparsed = do
  originalType <- cast original
  reparsedType <- cast reparsed
  pure $
    case (originalType, reparsedType) of
      (TAnn originalAnn originalInner, TAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (TAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedType) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedType
      (_, TAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalType reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalType reparsedType

collectPattern :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectPattern renderAnnotation ambient original reparsed = do
  originalPattern <- cast original
  reparsedPattern <- cast reparsed
  pure $
    case (originalPattern, reparsedPattern) of
      (PAnn originalAnn originalInner, PAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (PAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedPattern) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedPattern
      (_, PAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalPattern reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalPattern reparsedPattern

collectDecl :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectDecl renderAnnotation ambient original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure $
    case (originalDecl, reparsedDecl) of
      (DeclAnn originalAnn originalInner, DeclAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (DeclAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedDecl) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedDecl
      (_, DeclAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalDecl reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalDecl reparsedDecl

collectDataConDecl :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectDataConDecl renderAnnotation ambient original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure $
    case (originalDecl, reparsedDecl) of
      (DataConAnn originalAnn originalInner, DataConAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (DataConAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedDecl) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedDecl
      (_, DataConAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalDecl reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalDecl reparsedDecl

collectLiteral :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectLiteral renderAnnotation ambient original reparsed = do
  originalLiteral <- cast original
  reparsedLiteral <- cast reparsed
  pure $
    case (originalLiteral, reparsedLiteral) of
      (LitAnn originalAnn originalInner, LitAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (LitAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedLiteral) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedLiteral
      (_, LitAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalLiteral reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalLiteral reparsedLiteral

collectGuardQualifier :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectGuardQualifier renderAnnotation ambient original reparsed = do
  originalQualifier <- cast original
  reparsedQualifier <- cast reparsed
  pure $
    case (originalQualifier, reparsedQualifier) of
      (GuardAnn originalAnn originalInner, GuardAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (GuardAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedQualifier) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedQualifier
      (_, GuardAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalQualifier reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalQualifier reparsedQualifier

collectDoStmtExpr :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectDoStmtExpr renderAnnotation ambient original reparsed = do
  originalStmt <- cast original :: Maybe (DoStmt Expr)
  reparsedStmt <- cast reparsed :: Maybe (DoStmt Expr)
  pure (collectDoStmtPair renderAnnotation ambient originalStmt reparsedStmt)

collectDoStmtCmd :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectDoStmtCmd renderAnnotation ambient original reparsed = do
  originalStmt <- cast original :: Maybe (DoStmt Cmd)
  reparsedStmt <- cast reparsed :: Maybe (DoStmt Cmd)
  pure (collectDoStmtPair renderAnnotation ambient originalStmt reparsedStmt)

collectDoStmtPair :: (Data body) => AnnotationRenderer ann -> Maybe SourceSpan -> DoStmt body -> DoStmt body -> [PlacedLabel]
collectDoStmtPair renderAnnotation ambient originalStmt reparsedStmt =
  case (originalStmt, reparsedStmt) of
    (DoAnn originalAnn originalInner, DoAnn reparsedAnn reparsedInner) ->
      labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
        <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
    (DoAnn originalAnn originalInner, _) ->
      labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedStmt) ambient originalInner [originalAnn]
        <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedStmt
    (_, DoAnn _ reparsedInner) ->
      collectPair renderAnnotation ambient originalStmt reparsedInner
    _ -> collectGeneric renderAnnotation ambient originalStmt reparsedStmt

collectCompStmt :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectCompStmt renderAnnotation ambient original reparsed = do
  originalStmt <- cast original
  reparsedStmt <- cast reparsed
  pure $
    case (originalStmt, reparsedStmt) of
      (CompAnn originalAnn originalInner, CompAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (CompAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedStmt) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedStmt
      (_, CompAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalStmt reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalStmt reparsedStmt

collectArithSeq :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectArithSeq renderAnnotation ambient original reparsed = do
  originalSeq <- cast original
  reparsedSeq <- cast reparsed
  pure $
    case (originalSeq, reparsedSeq) of
      (ArithSeqAnn originalAnn originalInner, ArithSeqAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (ArithSeqAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedSeq) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedSeq
      (_, ArithSeqAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalSeq reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalSeq reparsedSeq

collectClassDeclItem :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectClassDeclItem renderAnnotation ambient original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (ClassItemAnn originalAnn originalInner, ClassItemAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (ClassItemAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedItem) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedItem
      (_, ClassItemAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalItem reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalItem reparsedItem

collectInstanceDeclItem :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectInstanceDeclItem renderAnnotation ambient original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (InstanceItemAnn originalAnn originalInner, InstanceItemAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (InstanceItemAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedItem) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedItem
      (_, InstanceItemAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalItem reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalItem reparsedItem

collectCmd :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectCmd renderAnnotation ambient original reparsed = do
  originalCmd <- cast original
  reparsedCmd <- cast reparsed
  pure $
    case (originalCmd, reparsedCmd) of
      (CmdAnn originalAnn originalInner, CmdAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (CmdAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedCmd) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedCmd
      (_, CmdAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalCmd reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalCmd reparsedCmd

collectExportSpec :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectExportSpec renderAnnotation ambient original reparsed = do
  originalSpec <- cast original
  reparsedSpec <- cast reparsed
  pure $
    case (originalSpec, reparsedSpec) of
      (ExportAnn originalAnn originalInner, ExportAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (ExportAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedSpec) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedSpec
      (_, ExportAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalSpec reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalSpec reparsedSpec

collectImportItem :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> Maybe [PlacedLabel]
collectImportItem renderAnnotation ambient original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (ImportAnn originalAnn originalInner, ImportAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpan reparsedAnn reparsedInner) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient reparsedAnn) originalInner reparsedInner
      (ImportAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (annotationCarrierSpan originalAnn reparsedItem) ambient originalInner [originalAnn]
          <> collectPair renderAnnotation (annotationAmbient ambient originalAnn) originalInner reparsedItem
      (_, ImportAnn _ reparsedInner) ->
        collectPair renderAnnotation ambient originalItem reparsedInner
      _ -> collectGeneric renderAnnotation ambient originalItem reparsedItem

collectGeneric :: (Data a) => AnnotationRenderer ann -> Maybe SourceSpan -> a -> a -> [PlacedLabel]
collectGeneric renderAnnotation ambient original reparsed
  | toConstr original /= toConstr reparsed =
      error
        ( "renderAnnotatedModule: AST constructor mismatch while merging annotations: "
            <> show (toConstr original)
            <> " /= "
            <> show (toConstr reparsed)
        )
  | length originalChildren /= length reparsedChildren =
      error
        ( "renderAnnotatedModule: AST child count mismatch while merging annotations at "
            <> show (toConstr original)
        )
  | otherwise =
      concat (zipWith collectChild originalChildren reparsedChildren)
  where
    originalChildren = gmapQ SomeData original
    reparsedChildren = gmapQ SomeData reparsed
    collectChild (SomeData originalChild) (SomeData reparsedChild) =
      case cast reparsedChild of
        Just typedReparsedChild -> collectPair renderAnnotation ambient originalChild typedReparsedChild
        Nothing ->
          error
            ( "renderAnnotatedModule: AST child type mismatch while merging annotations under "
                <> show (toConstr original)
            )

labelsAt :: (Data carrier) => AnnotationRenderer ann -> SourceSpan -> Maybe SourceSpan -> carrier -> [Annotation] -> [PlacedLabel]
labelsAt renderAnnotation span' ambient carrier annotations =
  [ PlacedLabel labelSpan label group
  | annotation <- annotations,
    AnnotationLabel labelSpan doc labelGroup <- renderAnnotation span' ambient carrier annotation,
    labelSpan /= NoSourceSpan,
    let label = renderLabelDoc doc,
    let group = labelGroup
  ]

renderLabelDoc :: Doc ann -> String
renderLabelDoc doc =
  let label = renderDoc doc
   in if '\n' `elem` label
        then error ("renderAnnotatedModule: multiline annotation label: " <> show label)
        else label

renderDoc :: Doc ann -> String
renderDoc =
  renderString . layoutPretty defaultLayoutOptions

spanFromAnnotations :: [Annotation] -> SourceSpan
spanFromAnnotations =
  fromMaybe NoSourceSpan . firstConcreteSpan . map spanFromAnnotation

spanFromAnnotation :: Annotation -> SourceSpan
spanFromAnnotation =
  fromMaybe NoSourceSpan . fromAnnotation @SourceSpan

annotationCarrierSpan :: (Data a) => Annotation -> a -> SourceSpan
annotationCarrierSpan annotation inner =
  case spanFromAnnotation annotation of
    NoSourceSpan -> firstSourceSpan inner
    span' -> span'

annotationAmbient :: Maybe SourceSpan -> Annotation -> Maybe SourceSpan
annotationAmbient ambient annotation =
  concreteSpan (spanFromAnnotation annotation) <|> ambient

firstSourceSpan :: (Data a) => a -> SourceSpan
firstSourceSpan node =
  fromMaybe NoSourceSpan $
    concreteSpan ownSpan
      <|> firstConcreteSpan (gmapQ firstSourceSpan node)
  where
    ownSpan =
      fromMaybe NoSourceSpan $
        (spanFromAnnotation <$> cast node)
          <|> (spanFromAnnotations <$> cast node)

firstConcreteSpan :: [SourceSpan] -> Maybe SourceSpan
firstConcreteSpan =
  foldr ((<|>) . concreteSpan) Nothing

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan span' = Just span'

renderAnnotatedSource :: String -> [PlacedLabel] -> String
renderAnnotatedSource source labels =
  let grouped = groupByLine labels
   in intercalate "\n" (concatMap (renderSourceLine grouped) (zip [1 ..] (lines source)))

groupByLine :: [PlacedLabel] -> Map.Map Int [PlacedLabel]
groupByLine = foldr insertLabel Map.empty
  where
    insertLabel label acc =
      case placedSpan label of
        SourceSpan _ startLine _ _ _ _ _ ->
          Map.insertWith (<>) startLine [label] acc
        NoSourceSpan -> acc

renderSourceLine :: Map.Map Int [PlacedLabel] -> (Int, String) -> [String]
renderSourceLine grouped (lineNum, srcLine) =
  let labels = maybe [] (sortOn labelKey) (Map.lookup lineNum grouped)
   in srcLine : renderAnnotationLines labels

labelKey :: PlacedLabel -> (Int, String)
labelKey label =
  (labelStartCol label, placedText label)

labelStartCol :: PlacedLabel -> Int
labelStartCol label =
  case placedSpan label of
    SourceSpan _ _ startCol _ _ _ _ -> startCol
    NoSourceSpan -> maxBound

type AnnotationItem = (Int, Maybe String, String)

renderAnnotationLines :: [PlacedLabel] -> [String]
renderAnnotationLines [] = []
renderAnnotationLines labels =
  layoutAnnotationLines [(labelStartCol label - 1, placedGroup label, placedText label) | label <- labels]

layoutAnnotationLines :: [AnnotationItem] -> [String]
layoutAnnotationLines [] = []
layoutAnnotationLines items =
  let reversed = sortOn (Down . annotationItemCol) items
      (currentLine, deferred) = packLine reversed
   in renderAnnotationLine currentLine deferred : layoutAnnotationLines deferred

packLine :: [AnnotationItem] -> ([AnnotationItem], [AnnotationItem])
packLine [] = ([], [])
packLine (rightmost : rest) =
  let lineGroup = annotationItemGroup rightmost
      go _ [] fitted deferred = (fitted, deferred)
      go minCol (item@(col, _, label) : remaining) fitted deferred =
        let annotEnd = col + 3 + length label
            fitsBeforePlaced = annotEnd < minCol
            crossesDeferred = any ((\d -> d > col && d < annotEnd) . annotationItemCol) deferred
            sameGroup = annotationItemGroup item == lineGroup
         in if sameGroup && fitsBeforePlaced && not crossesDeferred
              then go col remaining (item : fitted) deferred
              else go minCol remaining fitted (item : deferred)
   in go (annotationItemCol rightmost) rest [rightmost] []

annotationItemCol :: AnnotationItem -> Int
annotationItemCol (col, _, _) = col

annotationItemGroup :: AnnotationItem -> Maybe String
annotationItemGroup (_, group, _) = group

renderAnnotationLine :: [AnnotationItem] -> [AnnotationItem] -> String
renderAnnotationLine placedOnLine deferredItems =
  let deferredCols = map annotationItemCol deferredItems
      buildLine _ [] [] = ""
      buildLine pos placed deferred =
        case (placed, deferred) of
          ((col, _, label) : restPlaced, _)
            | pos == col ->
                "\x2514\x2500 " <> label <> buildLine (pos + 3 + length label) restPlaced (filter (>= pos + 3 + length label) deferred)
          (_, d : restDeferred)
            | pos == d ->
                "\x2502" <> buildLine (pos + 1) placed restDeferred
          _ ->
            let nextPos = case (placed, deferred) of
                  ((col, _, _) : _, d : _) -> min col d
                  ((col, _, _) : _, []) -> col
                  ([], d : _) -> d
                padding = nextPos - pos
             in replicate padding ' ' <> buildLine nextPos placed deferred
   in buildLine 0 (sortOn annotationItemCol placedOnLine) (sortOn id deferredCols)
