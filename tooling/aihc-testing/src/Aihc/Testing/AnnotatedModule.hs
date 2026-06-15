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

collectWrapped ::
  (Data node) =>
  (Annotation -> Maybe (Doc ann)) ->
  (node -> Maybe (Annotation, node)) ->
  node ->
  node ->
  [PlacedLabel]
collectWrapped renderAnnotation peel original reparsed =
  labelsAt renderAnnotation (spanFromAnnotations reparsedAnns) originalAnns
    <> collectGeneric renderAnnotation originalBase reparsedBase
  where
    (originalAnns, originalBase) = peelLeading peel original
    (reparsedAnns, reparsedBase) = peelLeading peel reparsed

peelLeading :: (node -> Maybe (Annotation, node)) -> node -> ([Annotation], node)
peelLeading peel =
  go []
  where
    go anns node =
      case peel node of
        Just (ann, inner) -> go (ann : anns) inner
        Nothing -> (reverse anns, node)

collectExpr :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectExpr renderAnnotation original reparsed = do
  originalExpr <- cast original
  reparsedExpr <- cast reparsed
  pure (collectWrapped renderAnnotation peelExprAnn originalExpr reparsedExpr)
  where
    peelExprAnn (EAnn ann inner) = Just (ann, inner)
    peelExprAnn _ = Nothing

collectType :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectType renderAnnotation original reparsed = do
  originalType <- cast original
  reparsedType <- cast reparsed
  pure (collectWrapped renderAnnotation peelTypeAnn originalType reparsedType)
  where
    peelTypeAnn (TAnn ann inner) = Just (ann, inner)
    peelTypeAnn _ = Nothing

collectPattern :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectPattern renderAnnotation original reparsed = do
  originalPattern <- cast original
  reparsedPattern <- cast reparsed
  pure (collectWrapped renderAnnotation peelPatternAnn originalPattern reparsedPattern)
  where
    peelPatternAnn (PAnn ann inner) = Just (ann, inner)
    peelPatternAnn _ = Nothing

collectDecl :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDecl renderAnnotation original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure (collectWrapped renderAnnotation peelDeclAnn originalDecl reparsedDecl)
  where
    peelDeclAnn (DeclAnn ann inner) = Just (ann, inner)
    peelDeclAnn _ = Nothing

collectDataConDecl :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectDataConDecl renderAnnotation original reparsed = do
  originalDecl <- cast original
  reparsedDecl <- cast reparsed
  pure (collectWrapped renderAnnotation peelDataConAnn originalDecl reparsedDecl)
  where
    peelDataConAnn (DataConAnn ann inner) = Just (ann, inner)
    peelDataConAnn _ = Nothing

collectLiteral :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectLiteral renderAnnotation original reparsed = do
  originalLiteral <- cast original
  reparsedLiteral <- cast reparsed
  pure (collectWrapped renderAnnotation peelLiteralAnn originalLiteral reparsedLiteral)
  where
    peelLiteralAnn (LitAnn ann inner) = Just (ann, inner)
    peelLiteralAnn _ = Nothing

collectGuardQualifier :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectGuardQualifier renderAnnotation original reparsed = do
  originalQualifier <- cast original
  reparsedQualifier <- cast reparsed
  pure (collectWrapped renderAnnotation peelGuardAnn originalQualifier reparsedQualifier)
  where
    peelGuardAnn (GuardAnn ann inner) = Just (ann, inner)
    peelGuardAnn _ = Nothing

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
collectDoStmtPair renderAnnotation =
  collectWrapped renderAnnotation peelDoAnn
  where
    peelDoAnn (DoAnn ann inner) = Just (ann, inner)
    peelDoAnn _ = Nothing

collectCompStmt :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectCompStmt renderAnnotation original reparsed = do
  originalStmt <- cast original
  reparsedStmt <- cast reparsed
  pure (collectWrapped renderAnnotation peelCompAnn originalStmt reparsedStmt)
  where
    peelCompAnn (CompAnn ann inner) = Just (ann, inner)
    peelCompAnn _ = Nothing

collectArithSeq :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectArithSeq renderAnnotation original reparsed = do
  originalSeq <- cast original
  reparsedSeq <- cast reparsed
  pure (collectWrapped renderAnnotation peelArithSeqAnn originalSeq reparsedSeq)
  where
    peelArithSeqAnn (ArithSeqAnn ann inner) = Just (ann, inner)
    peelArithSeqAnn _ = Nothing

collectClassDeclItem :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectClassDeclItem renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure (collectWrapped renderAnnotation peelClassItemAnn originalItem reparsedItem)
  where
    peelClassItemAnn (ClassItemAnn ann inner) = Just (ann, inner)
    peelClassItemAnn _ = Nothing

collectInstanceDeclItem :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectInstanceDeclItem renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure (collectWrapped renderAnnotation peelInstanceItemAnn originalItem reparsedItem)
  where
    peelInstanceItemAnn (InstanceItemAnn ann inner) = Just (ann, inner)
    peelInstanceItemAnn _ = Nothing

collectCmd :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectCmd renderAnnotation original reparsed = do
  originalCmd <- cast original
  reparsedCmd <- cast reparsed
  pure (collectWrapped renderAnnotation peelCmdAnn originalCmd reparsedCmd)
  where
    peelCmdAnn (CmdAnn ann inner) = Just (ann, inner)
    peelCmdAnn _ = Nothing

collectExportSpec :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectExportSpec renderAnnotation original reparsed = do
  originalSpec <- cast original
  reparsedSpec <- cast reparsed
  pure (collectWrapped renderAnnotation peelExportAnn originalSpec reparsedSpec)
  where
    peelExportAnn (ExportAnn ann inner) = Just (ann, inner)
    peelExportAnn _ = Nothing

collectImportItem :: (Data a) => (Annotation -> Maybe (Doc ann)) -> a -> a -> Maybe [PlacedLabel]
collectImportItem renderAnnotation original reparsed = do
  originalItem <- cast original
  reparsedItem <- cast reparsed
  pure (collectWrapped renderAnnotation peelImportAnn originalItem reparsedItem)
  where
    peelImportAnn (ImportAnn ann inner) = Just (ann, inner)
    peelImportAnn _ = Nothing

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
labelsAt renderAnnotation span' =
  concatMap labelAt
  where
    labelAt annotation =
      case renderAnnotation annotation of
        Nothing -> []
        Just doc ->
          let label = renderLabelDoc doc
           in case span' of
                NoSourceSpan ->
                  error ("renderAnnotatedModule: renderable annotation has no source span: " <> show label)
                _ -> [PlacedLabel span' label]

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
