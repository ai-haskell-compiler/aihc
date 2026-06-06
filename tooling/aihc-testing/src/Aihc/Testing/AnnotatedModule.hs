{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Aihc.Testing.AnnotatedModule
  ( renderAnnotatedModule,
    renderAnnotatedModules,
    renderAnnotatedModuleSource,
    renderAnnotatedModuleSources,
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

renderAnnotatedModule :: ParserConfig -> (Annotation -> Maybe (Doc ann)) -> Module -> String
renderAnnotatedModule parserConfig renderAnnotation modu =
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

renderAnnotatedModules :: ParserConfig -> (Annotation -> Maybe (Doc ann)) -> [Module] -> [String]
renderAnnotatedModules parserConfig renderAnnotation =
  map (renderAnnotatedModule parserConfig renderAnnotation)

renderAnnotatedModuleSource :: (Annotation -> Maybe (Doc ann)) -> T.Text -> Module -> String
renderAnnotatedModuleSource renderAnnotation source modu =
  length labels `seq` renderAnnotatedSource (T.unpack source) labels
  where
    labels = collectLabels renderAnnotation modu modu

renderAnnotatedModuleSources :: (Annotation -> Maybe (Doc ann)) -> [T.Text] -> [Module] -> [String]
renderAnnotatedModuleSources renderAnnotation sources modules =
  case compare (length sources) (length modules) of
    LT -> error "renderAnnotatedModuleSources: fewer source texts than modules"
    GT -> error "renderAnnotatedModuleSources: more source texts than modules"
    EQ -> zipWith (renderAnnotatedModuleSource renderAnnotation) sources modules

data PlacedLabel = PlacedLabel
  { placedSpan :: !SourceSpan,
    placedText :: !String
  }
  deriving (Eq, Show)

data SomeData = forall a. (Data a) => SomeData a

type SpanContext = Maybe SourceSpan

collectLabels :: (Annotation -> Maybe (Doc ann)) -> Module -> Module -> [PlacedLabel]
collectLabels = collectPairWith Nothing

collectPairWith :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> [PlacedLabel]
collectPairWith context renderAnnotation original reparsed =
  fromMaybe (collectGeneric context renderAnnotation original reparsed) (collectSpecial context renderAnnotation original reparsed)

collectSpecial :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectSpecial context renderAnnotation original reparsed =
  collectAnnotationList context renderAnnotation original reparsed
    <|> collectAnnotation context renderAnnotation original reparsed
    <|> collectExpr context renderAnnotation original reparsed
    <|> collectType context renderAnnotation original reparsed
    <|> collectPattern context renderAnnotation original reparsed
    <|> collectDecl context renderAnnotation original reparsed
    <|> collectDataConDecl context renderAnnotation original reparsed
    <|> collectLiteral context renderAnnotation original reparsed
    <|> collectGuardQualifier context renderAnnotation original reparsed
    <|> collectDoStmtExpr context renderAnnotation original reparsed
    <|> collectDoStmtCmd context renderAnnotation original reparsed
    <|> collectCompStmt context renderAnnotation original reparsed
    <|> collectArithSeq context renderAnnotation original reparsed
    <|> collectClassDeclItem context renderAnnotation original reparsed
    <|> collectInstanceDeclItem context renderAnnotation original reparsed
    <|> collectCmd context renderAnnotation original reparsed
    <|> collectExportSpec context renderAnnotation original reparsed
    <|> collectImportItem context renderAnnotation original reparsed

collectAnnotationList :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectAnnotationList context renderAnnotation original reparsed = do
  originalAnns <- cast original
  reparsedAnns <- cast reparsed
  pure (labelsAt renderAnnotation (spanFromAnnotationsInContext context reparsedAnns) originalAnns)

collectAnnotation :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectAnnotation context renderAnnotation original reparsed = do
  originalAnn <- cast original
  reparsedAnn <- cast reparsed
  pure (labelsAt renderAnnotation (annotationSpanInContext context reparsedAnn) [originalAnn])

collectExpr :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectExpr context renderAnnotation original reparsed = do
  originalExpr <- cast original
  reparsedExpr <- cast reparsed
  pure $
    case (originalExpr, reparsedExpr) of
      (EAnn originalAnn originalInner, EAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (EAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedExpr
      (_, EAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalExpr reparsedInner
      _ -> collectGeneric context renderAnnotation originalExpr reparsedExpr

collectType :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectType context renderAnnotation original reparsed = do
  originalType <- cast original
  reparsedType <- cast reparsed
  pure $
    case (originalType, reparsedType) of
      (TAnn originalAnn originalInner, TAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (TAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedType
      (_, TAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalType reparsedInner
      _ -> collectGeneric context renderAnnotation originalType reparsedType

collectPattern :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectPattern context renderAnnotation original reparsed = do
  originalPattern <- cast original
  reparsedPattern <- cast reparsed
  pure $
    case (originalPattern, reparsedPattern) of
      (PAnn originalAnn originalInner, PAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (PAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedPattern
      (_, PAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalPattern reparsedInner
      _ -> collectGeneric context renderAnnotation originalPattern reparsedPattern

collectDecl :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDecl context renderAnnotation original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure $
    case (originalDecl, reparsedDecl) of
      (DeclAnn originalAnn originalInner, DeclAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (DeclAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedDecl
      (_, DeclAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalDecl reparsedInner
      _ -> collectGeneric context renderAnnotation originalDecl reparsedDecl

collectDataConDecl :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDataConDecl context renderAnnotation original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure $
    case (originalDecl, reparsedDecl) of
      (DataConAnn originalAnn originalInner, DataConAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (DataConAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedDecl
      (_, DataConAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalDecl reparsedInner
      _ -> collectGeneric context renderAnnotation originalDecl reparsedDecl

collectLiteral :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectLiteral context renderAnnotation original reparsed = do
  originalLiteral <- cast original
  reparsedLiteral <- cast reparsed
  pure $
    case (originalLiteral, reparsedLiteral) of
      (LitAnn originalAnn originalInner, LitAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (LitAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedLiteral
      (_, LitAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalLiteral reparsedInner
      _ -> collectGeneric context renderAnnotation originalLiteral reparsedLiteral

collectGuardQualifier :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectGuardQualifier context renderAnnotation original reparsed = do
  originalQualifier <- cast original
  reparsedQualifier <- cast reparsed
  pure $
    case (originalQualifier, reparsedQualifier) of
      (GuardAnn originalAnn originalInner, GuardAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (GuardAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedQualifier
      (_, GuardAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalQualifier reparsedInner
      _ -> collectGeneric context renderAnnotation originalQualifier reparsedQualifier

collectDoStmtExpr :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDoStmtExpr context renderAnnotation original reparsed = do
  originalStmt <- cast original :: Maybe (DoStmt Expr)
  reparsedStmt <- cast reparsed :: Maybe (DoStmt Expr)
  pure (collectDoStmtPair context renderAnnotation originalStmt reparsedStmt)

collectDoStmtCmd :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDoStmtCmd context renderAnnotation original reparsed = do
  originalStmt <- cast original :: Maybe (DoStmt Cmd)
  reparsedStmt <- cast reparsed :: Maybe (DoStmt Cmd)
  pure (collectDoStmtPair context renderAnnotation originalStmt reparsedStmt)

collectDoStmtPair :: (Data body) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> DoStmt body -> DoStmt body -> [PlacedLabel]
collectDoStmtPair context renderAnnotation originalStmt reparsedStmt =
  case (originalStmt, reparsedStmt) of
    (DoAnn originalAnn originalInner, DoAnn reparsedAnn reparsedInner) ->
      labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
        <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
    (DoAnn originalAnn originalInner, _) ->
      labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
        <> collectPairWith context renderAnnotation originalInner reparsedStmt
    (_, DoAnn reparsedAnn reparsedInner) ->
      collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalStmt reparsedInner
    _ -> collectGeneric context renderAnnotation originalStmt reparsedStmt

collectCompStmt :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectCompStmt context renderAnnotation original reparsed = do
  originalStmt <- cast original
  reparsedStmt <- cast reparsed
  pure $
    case (originalStmt, reparsedStmt) of
      (CompAnn originalAnn originalInner, CompAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (CompAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedStmt
      (_, CompAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalStmt reparsedInner
      _ -> collectGeneric context renderAnnotation originalStmt reparsedStmt

collectArithSeq :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectArithSeq context renderAnnotation original reparsed = do
  originalSeq <- cast original
  reparsedSeq <- cast reparsed
  pure $
    case (originalSeq, reparsedSeq) of
      (ArithSeqAnn originalAnn originalInner, ArithSeqAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (ArithSeqAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedSeq
      (_, ArithSeqAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalSeq reparsedInner
      _ -> collectGeneric context renderAnnotation originalSeq reparsedSeq

collectClassDeclItem :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectClassDeclItem context renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (ClassItemAnn originalAnn originalInner, ClassItemAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (ClassItemAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedItem
      (_, ClassItemAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalItem reparsedInner
      _ -> collectGeneric context renderAnnotation originalItem reparsedItem

collectInstanceDeclItem :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectInstanceDeclItem context renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (InstanceItemAnn originalAnn originalInner, InstanceItemAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (InstanceItemAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedItem
      (_, InstanceItemAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalItem reparsedInner
      _ -> collectGeneric context renderAnnotation originalItem reparsedItem

collectCmd :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectCmd context renderAnnotation original reparsed = do
  originalCmd <- cast original
  reparsedCmd <- cast reparsed
  pure $
    case (originalCmd, reparsedCmd) of
      (CmdAnn originalAnn originalInner, CmdAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (CmdAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedCmd
      (_, CmdAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalCmd reparsedInner
      _ -> collectGeneric context renderAnnotation originalCmd reparsedCmd

collectExportSpec :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectExportSpec context renderAnnotation original reparsed = do
  originalSpec <- cast original
  reparsedSpec <- cast reparsed
  pure $
    case (originalSpec, reparsedSpec) of
      (ExportAnn originalAnn originalInner, ExportAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (ExportAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedSpec
      (_, ExportAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalSpec reparsedInner
      _ -> collectGeneric context renderAnnotation originalSpec reparsedSpec

collectImportItem :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectImportItem context renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (ImportAnn originalAnn originalInner, ImportAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (annotationCarrierSpanInContext context reparsedAnn reparsedInner) [originalAnn]
          <> collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalInner reparsedInner
      (ImportAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (contextSourceSpan context) [originalAnn]
          <> collectPairWith context renderAnnotation originalInner reparsedItem
      (_, ImportAnn reparsedAnn reparsedInner) ->
        collectPairWith (pushAnnotationSpan context reparsedAnn) renderAnnotation originalItem reparsedInner
      _ -> collectGeneric context renderAnnotation originalItem reparsedItem

collectGeneric :: (Data a) => SpanContext -> (Annotation -> Maybe (Doc ann)) -> a -> a -> [PlacedLabel]
collectGeneric context renderAnnotation original reparsed
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
        Just typedReparsedChild -> collectPairWith context renderAnnotation originalChild typedReparsedChild
        Nothing ->
          error
            ( "renderAnnotatedModule: AST child type mismatch while merging annotations under "
                <> show (toConstr original)
            )

labelsAt :: (Annotation -> Maybe (Doc ann)) -> SourceSpan -> [Annotation] -> [PlacedLabel]
labelsAt renderAnnotation span' annotations =
  case (span', labels) of
    (NoSourceSpan, []) -> []
    (NoSourceSpan, _) ->
      error ("renderAnnotatedModule: renderable annotation has no source span: " <> show labels)
    _ -> [PlacedLabel span' label | label <- labels]
  where
    labels =
      [ label
      | annotation <- annotations,
        Just doc <- [renderAnnotation annotation],
        let label = renderLabelDoc doc
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

contextSourceSpan :: SpanContext -> SourceSpan
contextSourceSpan =
  fromMaybe NoSourceSpan

spanFromAnnotation :: Annotation -> SourceSpan
spanFromAnnotation =
  fromMaybe NoSourceSpan . fromAnnotation @SourceSpan

annotationSpanInContext :: SpanContext -> Annotation -> SourceSpan
annotationSpanInContext context annotation =
  spanFromAnnotation annotation `orSourceSpan` contextSourceSpan context

annotationCarrierSpanInContext :: (Data a) => SpanContext -> Annotation -> a -> SourceSpan
annotationCarrierSpanInContext context annotation inner =
  spanFromAnnotation annotation `orSourceSpan` contextSourceSpan (annotationWrapperSpan inner <|> context)

spanFromAnnotationsInContext :: SpanContext -> [Annotation] -> SourceSpan
spanFromAnnotationsInContext context annotations =
  fromMaybe (contextSourceSpan context) (firstConcreteSpan (map spanFromAnnotation annotations))

pushAnnotationSpan :: SpanContext -> Annotation -> SpanContext
pushAnnotationSpan context annotation =
  concreteSpan (spanFromAnnotation annotation) <|> context

annotationWrapperSpan :: (Data a) => a -> Maybe SourceSpan
annotationWrapperSpan node =
  exprAnnSpan
    <|> typeAnnSpan
    <|> patternAnnSpan
    <|> declAnnSpan
    <|> dataConAnnSpan
    <|> literalAnnSpan
    <|> guardAnnSpan
    <|> doStmtExprAnnSpan
    <|> doStmtCmdAnnSpan
    <|> compStmtAnnSpan
    <|> arithSeqAnnSpan
    <|> classItemAnnSpan
    <|> instanceItemAnnSpan
    <|> cmdAnnSpan
    <|> exportAnnSpan
    <|> importAnnSpan
  where
    exprAnnSpan = cast node >>= exprSpanFromWrapper
    typeAnnSpan = cast node >>= typeSpanFromWrapper
    patternAnnSpan = cast node >>= patternSpanFromWrapper
    declAnnSpan = cast node >>= declSpanFromWrapper
    dataConAnnSpan = cast node >>= dataConSpanFromWrapper
    literalAnnSpan = cast node >>= literalSpanFromWrapper
    guardAnnSpan = cast node >>= guardSpanFromWrapper
    doStmtExprAnnSpan = (cast node :: Maybe (DoStmt Expr)) >>= doStmtSpanFromWrapper
    doStmtCmdAnnSpan = (cast node :: Maybe (DoStmt Cmd)) >>= doStmtSpanFromWrapper
    compStmtAnnSpan = cast node >>= compStmtSpanFromWrapper
    arithSeqAnnSpan = cast node >>= arithSeqSpanFromWrapper
    classItemAnnSpan = cast node >>= classItemSpanFromWrapper
    instanceItemAnnSpan = cast node >>= instanceItemSpanFromWrapper
    cmdAnnSpan = cast node >>= cmdSpanFromWrapper
    exportAnnSpan = cast node >>= exportSpanFromWrapper
    importAnnSpan = cast node >>= importSpanFromWrapper

exprSpanFromWrapper :: Expr -> Maybe SourceSpan
exprSpanFromWrapper (EAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
exprSpanFromWrapper _ = Nothing

typeSpanFromWrapper :: Type -> Maybe SourceSpan
typeSpanFromWrapper (TAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
typeSpanFromWrapper _ = Nothing

patternSpanFromWrapper :: Pattern -> Maybe SourceSpan
patternSpanFromWrapper (PAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
patternSpanFromWrapper _ = Nothing

declSpanFromWrapper :: Decl -> Maybe SourceSpan
declSpanFromWrapper (DeclAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
declSpanFromWrapper _ = Nothing

dataConSpanFromWrapper :: DataConDecl -> Maybe SourceSpan
dataConSpanFromWrapper (DataConAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
dataConSpanFromWrapper _ = Nothing

literalSpanFromWrapper :: Literal -> Maybe SourceSpan
literalSpanFromWrapper (LitAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
literalSpanFromWrapper _ = Nothing

guardSpanFromWrapper :: GuardQualifier -> Maybe SourceSpan
guardSpanFromWrapper (GuardAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
guardSpanFromWrapper _ = Nothing

doStmtSpanFromWrapper :: (Data body) => DoStmt body -> Maybe SourceSpan
doStmtSpanFromWrapper (DoAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
doStmtSpanFromWrapper _ = Nothing

compStmtSpanFromWrapper :: CompStmt -> Maybe SourceSpan
compStmtSpanFromWrapper (CompAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
compStmtSpanFromWrapper _ = Nothing

arithSeqSpanFromWrapper :: ArithSeq -> Maybe SourceSpan
arithSeqSpanFromWrapper (ArithSeqAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
arithSeqSpanFromWrapper _ = Nothing

classItemSpanFromWrapper :: ClassDeclItem -> Maybe SourceSpan
classItemSpanFromWrapper (ClassItemAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
classItemSpanFromWrapper _ = Nothing

instanceItemSpanFromWrapper :: InstanceDeclItem -> Maybe SourceSpan
instanceItemSpanFromWrapper (InstanceItemAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
instanceItemSpanFromWrapper _ = Nothing

cmdSpanFromWrapper :: Cmd -> Maybe SourceSpan
cmdSpanFromWrapper (CmdAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
cmdSpanFromWrapper _ = Nothing

exportSpanFromWrapper :: ExportSpec -> Maybe SourceSpan
exportSpanFromWrapper (ExportAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
exportSpanFromWrapper _ = Nothing

importSpanFromWrapper :: ImportItem -> Maybe SourceSpan
importSpanFromWrapper (ImportAnn ann inner) = concreteSpan (spanFromAnnotation ann) <|> annotationWrapperSpan inner
importSpanFromWrapper _ = Nothing

firstConcreteSpan :: [SourceSpan] -> Maybe SourceSpan
firstConcreteSpan =
  foldr ((<|>) . concreteSpan) Nothing

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan span' = Just span'

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan contextSpan = contextSpan
orSourceSpan span' _contextSpan = span'

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

type AnnotationItem = (Int, String)

renderAnnotationLines :: [PlacedLabel] -> [String]
renderAnnotationLines [] = []
renderAnnotationLines labels =
  layoutAnnotationLines [(labelStartCol label - 1, placedText label) | label <- labels]

layoutAnnotationLines :: [AnnotationItem] -> [String]
layoutAnnotationLines [] = []
layoutAnnotationLines items =
  let reversed = sortOn (Down . fst) items
      (currentLine, deferred) = packLine reversed
   in renderAnnotationLine currentLine deferred : layoutAnnotationLines deferred

packLine :: [AnnotationItem] -> ([AnnotationItem], [AnnotationItem])
packLine [] = ([], [])
packLine (rightmost : rest) =
  let go _ [] fitted deferred = (fitted, deferred)
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
