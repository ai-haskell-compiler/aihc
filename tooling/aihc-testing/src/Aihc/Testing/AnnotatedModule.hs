{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Aihc.Testing.AnnotatedModule
  ( renderAnnotatedModule,
    renderAnnotatedModules,
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

data PlacedLabel = PlacedLabel
  { placedSpan :: !SourceSpan,
    placedText :: !String
  }
  deriving (Eq, Show)

data SomeData = forall a. (Data a) => SomeData a

collectLabels :: (Annotation -> Maybe (Doc ann)) -> Module -> Module -> [PlacedLabel]
collectLabels = collectPair

collectPair :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> [PlacedLabel]
collectPair renderAnnotation original reparsed =
  fromMaybe (collectGeneric renderAnnotation original reparsed) (collectSpecial renderAnnotation original reparsed)

collectSpecial :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectSpecial renderAnnotation original reparsed =
  collectAnnotationList renderAnnotation original reparsed
    <|> collectAnnotation renderAnnotation original reparsed
    <|> collectExpr renderAnnotation original reparsed
    <|> collectType renderAnnotation original reparsed
    <|> collectPattern renderAnnotation original reparsed
    <|> collectDecl renderAnnotation original reparsed
    <|> collectDataConDecl renderAnnotation original reparsed
    <|> collectLiteral renderAnnotation original reparsed
    <|> collectGuardQualifier renderAnnotation original reparsed
    <|> collectDoStmtExpr renderAnnotation original reparsed
    <|> collectDoStmtCmd renderAnnotation original reparsed
    <|> collectCompStmt renderAnnotation original reparsed
    <|> collectArithSeq renderAnnotation original reparsed
    <|> collectClassDeclItem renderAnnotation original reparsed
    <|> collectInstanceDeclItem renderAnnotation original reparsed
    <|> collectCmd renderAnnotation original reparsed
    <|> collectExportSpec renderAnnotation original reparsed
    <|> collectImportItem renderAnnotation original reparsed

collectAnnotationList :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectAnnotationList renderAnnotation original reparsed = do
  originalAnns <- cast original
  reparsedAnns <- cast reparsed
  pure (labelsAt renderAnnotation (spanFromAnnotations reparsedAnns) originalAnns)

collectAnnotation :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectAnnotation renderAnnotation original reparsed = do
  originalAnn <- cast original
  reparsedAnn <- cast reparsed
  pure (labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn])

collectExpr :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectExpr renderAnnotation original reparsed = do
  originalExpr <- cast original
  reparsedExpr <- cast reparsed
  pure $
    case (originalExpr, reparsedExpr) of
      (EAnn originalAnn originalInner, EAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (EAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedExpr) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedExpr
      (_, EAnn _ reparsedInner) ->
        collectPair renderAnnotation originalExpr reparsedInner
      _ -> collectGeneric renderAnnotation originalExpr reparsedExpr

collectType :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectType renderAnnotation original reparsed = do
  originalType <- cast original
  reparsedType <- cast reparsed
  pure $
    case (originalType, reparsedType) of
      (TAnn originalAnn originalInner, TAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (TAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedType) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedType
      (_, TAnn _ reparsedInner) ->
        collectPair renderAnnotation originalType reparsedInner
      _ -> collectGeneric renderAnnotation originalType reparsedType

collectPattern :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectPattern renderAnnotation original reparsed = do
  originalPattern <- cast original
  reparsedPattern <- cast reparsed
  pure $
    case (originalPattern, reparsedPattern) of
      (PAnn originalAnn originalInner, PAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (PAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedPattern) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedPattern
      (_, PAnn _ reparsedInner) ->
        collectPair renderAnnotation originalPattern reparsedInner
      _ -> collectGeneric renderAnnotation originalPattern reparsedPattern

collectDecl :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDecl renderAnnotation original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure $
    case (originalDecl, reparsedDecl) of
      (DeclAnn originalAnn originalInner, DeclAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (DeclAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedDecl) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedDecl
      (_, DeclAnn _ reparsedInner) ->
        collectPair renderAnnotation originalDecl reparsedInner
      _ -> collectGeneric renderAnnotation originalDecl reparsedDecl

collectDataConDecl :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDataConDecl renderAnnotation original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure $
    case (originalDecl, reparsedDecl) of
      (DataConAnn originalAnn originalInner, DataConAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (DataConAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedDecl) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedDecl
      (_, DataConAnn _ reparsedInner) ->
        collectPair renderAnnotation originalDecl reparsedInner
      _ -> collectGeneric renderAnnotation originalDecl reparsedDecl

collectLiteral :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectLiteral renderAnnotation original reparsed = do
  originalLiteral <- cast original
  reparsedLiteral <- cast reparsed
  pure $
    case (originalLiteral, reparsedLiteral) of
      (LitAnn originalAnn originalInner, LitAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (LitAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedLiteral) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedLiteral
      (_, LitAnn _ reparsedInner) ->
        collectPair renderAnnotation originalLiteral reparsedInner
      _ -> collectGeneric renderAnnotation originalLiteral reparsedLiteral

collectGuardQualifier :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectGuardQualifier renderAnnotation original reparsed = do
  originalQualifier <- cast original
  reparsedQualifier <- cast reparsed
  pure $
    case (originalQualifier, reparsedQualifier) of
      (GuardAnn originalAnn originalInner, GuardAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (GuardAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedQualifier) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedQualifier
      (_, GuardAnn _ reparsedInner) ->
        collectPair renderAnnotation originalQualifier reparsedInner
      _ -> collectGeneric renderAnnotation originalQualifier reparsedQualifier

collectDoStmtExpr :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDoStmtExpr renderAnnotation original reparsed = do
  originalStmt <- cast original :: Maybe (DoStmt Expr)
  reparsedStmt <- cast reparsed :: Maybe (DoStmt Expr)
  pure (collectDoStmtPair renderAnnotation originalStmt reparsedStmt)

collectDoStmtCmd :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDoStmtCmd renderAnnotation original reparsed = do
  originalStmt <- cast original :: Maybe (DoStmt Cmd)
  reparsedStmt <- cast reparsed :: Maybe (DoStmt Cmd)
  pure (collectDoStmtPair renderAnnotation originalStmt reparsedStmt)

collectDoStmtPair :: (Data body) => (Annotation -> Maybe (Doc ann)) -> DoStmt body -> DoStmt body -> [PlacedLabel]
collectDoStmtPair renderAnnotation originalStmt reparsedStmt =
  case (originalStmt, reparsedStmt) of
    (DoAnn originalAnn originalInner, DoAnn reparsedAnn reparsedInner) ->
      labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
        <> collectPair renderAnnotation originalInner reparsedInner
    (DoAnn originalAnn originalInner, _) ->
      labelsAt renderAnnotation (firstSourceSpan reparsedStmt) [originalAnn]
        <> collectPair renderAnnotation originalInner reparsedStmt
    (_, DoAnn _ reparsedInner) ->
      collectPair renderAnnotation originalStmt reparsedInner
    _ -> collectGeneric renderAnnotation originalStmt reparsedStmt

collectCompStmt :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectCompStmt renderAnnotation original reparsed = do
  originalStmt <- cast original
  reparsedStmt <- cast reparsed
  pure $
    case (originalStmt, reparsedStmt) of
      (CompAnn originalAnn originalInner, CompAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (CompAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedStmt) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedStmt
      (_, CompAnn _ reparsedInner) ->
        collectPair renderAnnotation originalStmt reparsedInner
      _ -> collectGeneric renderAnnotation originalStmt reparsedStmt

collectArithSeq :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectArithSeq renderAnnotation original reparsed = do
  originalSeq <- cast original
  reparsedSeq <- cast reparsed
  pure $
    case (originalSeq, reparsedSeq) of
      (ArithSeqAnn originalAnn originalInner, ArithSeqAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (ArithSeqAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedSeq) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedSeq
      (_, ArithSeqAnn _ reparsedInner) ->
        collectPair renderAnnotation originalSeq reparsedInner
      _ -> collectGeneric renderAnnotation originalSeq reparsedSeq

collectClassDeclItem :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectClassDeclItem renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (ClassItemAnn originalAnn originalInner, ClassItemAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (ClassItemAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedItem) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedItem
      (_, ClassItemAnn _ reparsedInner) ->
        collectPair renderAnnotation originalItem reparsedInner
      _ -> collectGeneric renderAnnotation originalItem reparsedItem

collectInstanceDeclItem :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectInstanceDeclItem renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (InstanceItemAnn originalAnn originalInner, InstanceItemAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (InstanceItemAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedItem) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedItem
      (_, InstanceItemAnn _ reparsedInner) ->
        collectPair renderAnnotation originalItem reparsedInner
      _ -> collectGeneric renderAnnotation originalItem reparsedItem

collectCmd :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectCmd renderAnnotation original reparsed = do
  originalCmd <- cast original
  reparsedCmd <- cast reparsed
  pure $
    case (originalCmd, reparsedCmd) of
      (CmdAnn originalAnn originalInner, CmdAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (CmdAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedCmd) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedCmd
      (_, CmdAnn _ reparsedInner) ->
        collectPair renderAnnotation originalCmd reparsedInner
      _ -> collectGeneric renderAnnotation originalCmd reparsedCmd

collectExportSpec :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectExportSpec renderAnnotation original reparsed = do
  originalSpec <- cast original
  reparsedSpec <- cast reparsed
  pure $
    case (originalSpec, reparsedSpec) of
      (ExportAnn originalAnn originalInner, ExportAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (ExportAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedSpec) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedSpec
      (_, ExportAnn _ reparsedInner) ->
        collectPair renderAnnotation originalSpec reparsedInner
      _ -> collectGeneric renderAnnotation originalSpec reparsedSpec

collectImportItem :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectImportItem renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure $
    case (originalItem, reparsedItem) of
      (ImportAnn originalAnn originalInner, ImportAnn reparsedAnn reparsedInner) ->
        labelsAt renderAnnotation (spanFromAnnotation reparsedAnn) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedInner
      (ImportAnn originalAnn originalInner, _) ->
        labelsAt renderAnnotation (firstSourceSpan reparsedItem) [originalAnn]
          <> collectPair renderAnnotation originalInner reparsedItem
      (_, ImportAnn _ reparsedInner) ->
        collectPair renderAnnotation originalItem reparsedInner
      _ -> collectGeneric renderAnnotation originalItem reparsedItem

collectGeneric :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> [PlacedLabel]
collectGeneric renderAnnotation original reparsed
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
        Just typedReparsedChild -> collectPair renderAnnotation originalChild typedReparsedChild
        Nothing ->
          error
            ( "renderAnnotatedModule: AST child type mismatch while merging annotations under "
                <> show (toConstr original)
            )

labelsAt :: (Annotation -> Maybe (Doc ann)) -> SourceSpan -> [Annotation] -> [PlacedLabel]
labelsAt renderAnnotation span' annotations =
  [ PlacedLabel span' label
  | span' /= NoSourceSpan,
    annotation <- annotations,
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

spanFromAnnotations :: [Annotation] -> SourceSpan
spanFromAnnotations =
  fromMaybe NoSourceSpan . firstConcreteSpan . map spanFromAnnotation

spanFromAnnotation :: Annotation -> SourceSpan
spanFromAnnotation =
  fromMaybe NoSourceSpan . fromAnnotation @SourceSpan

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
