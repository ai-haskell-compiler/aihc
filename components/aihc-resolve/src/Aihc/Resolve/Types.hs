{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Aihc.Resolve.Types
  ( pattern DeclResolution,
    pattern EResolution,
    pattern PResolution,
    pattern TResolution,
    ResolutionNamespace (..),
    ResolvedName (..),
    ResolutionAnnotation (..),
    ResolveError (..),
    ResolveResult (..),
    renderResolveResult,
    renderResolvedName,
    renderAnnotatedSource,
    renderAnnotatedResolveResult,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    Module (..),
    Name,
    Pattern (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName,
    fromAnnotation,
    moduleName,
    renderName,
    renderUnqualifiedName,
  )
import Data.Data (Data, cast, gmapQ)
import Data.List (intercalate, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

data ResolvedName
  = ResolvedTopLevel Name
  | ResolvedLocal Int UnqualifiedName
  | ResolvedError String
  deriving (Eq, Show, Typeable)

data ResolutionNamespace
  = ResolutionNamespaceTerm
  | ResolutionNamespaceType
  deriving (Eq, Show, Typeable)

data ResolutionAnnotation = ResolutionAnnotation
  { resolutionSpan :: !SourceSpan,
    resolutionName :: !Text,
    resolutionNamespace :: !ResolutionNamespace,
    resolutionTarget :: !ResolvedName
  }
  deriving (Eq, Show, Typeable)

newtype ResolveError
  = ResolveNotImplemented String
  deriving (Eq, Show)

data ResolveResult = ResolveResult
  { resolvedModules :: [Module],
    resolvedAnnotations :: [(Text, [ResolutionAnnotation])],
    resolveErrors :: [ResolveError]
  }
  deriving (Show)

pattern DeclResolution :: ResolutionAnnotation -> Decl
pattern DeclResolution resolution <- DeclAnn (fromAnnotation -> Just resolution) _

pattern PResolution :: ResolutionAnnotation -> Pattern
pattern PResolution resolution <- PAnn (fromAnnotation -> Just resolution) _

pattern TResolution :: ResolutionAnnotation -> Type
pattern TResolution resolution <- TAnn (fromAnnotation -> Just resolution) _

pattern EResolution :: ResolutionAnnotation -> Expr
pattern EResolution resolution <- EAnn (fromAnnotation -> Just resolution) _

renderResolveResult :: ResolveResult -> String
renderResolveResult result =
  intercalate "\n" (map renderModuleAnnotations (sortOn fst (mergeModules (collectModules (resolvedModules result) <> resolvedAnnotations result))))

renderResolvedName :: ResolvedName -> String
renderResolvedName resolvedName =
  case resolvedName of
    ResolvedTopLevel name -> T.unpack (renderName name)
    ResolvedLocal uniqueId localName -> "Local " <> show uniqueId <> " " <> T.unpack (renderUnqualifiedName localName)
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
    <> patternResolution (cast node)
    <> typeResolution (cast node)
    <> exprResolution (cast node)

declResolution :: Maybe Decl -> [ResolutionAnnotation]
declResolution maybeDecl =
  case maybeDecl of
    Just (DeclResolution resolution) -> [resolution]
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

-- | Pretty-print module source text with resolution annotations interleaved.
--
-- Each annotation is rendered as a line below the source line it belongs to,
-- using box-drawing characters to point at the annotated identifier:
--
-- @
-- y = x
-- │   └─ (value) Main.x
-- └─ (value) Main.y
-- @
--
-- When multiple annotations for the same source line fit on one annotation
-- line (no overlap), they are combined:
--
-- @
-- y =                x
-- └─ (value) Main.y  └─ (value) Main.x
-- @
renderAnnotatedSource :: Text -> [ResolutionAnnotation] -> String
renderAnnotatedSource source annotations =
  let sourceLines = T.lines source
      sorted = sortOn annotationKey annotations
      grouped = groupByLine sorted
   in intercalate "\n" (concatMap (renderSourceLine grouped) (zip [1 ..] sourceLines))

-- | Group annotations by their start line number.
groupByLine :: [ResolutionAnnotation] -> Map.Map Int [ResolutionAnnotation]
groupByLine = foldr insertAnn Map.empty
  where
    insertAnn ann acc =
      case resolutionSpan ann of
        SourceSpan _ startLine _ _ _ _ _ ->
          Map.insertWith (<>) startLine [ann] acc
        NoSourceSpan -> acc

-- | Render a single source line and its annotations.
renderSourceLine :: Map.Map Int [ResolutionAnnotation] -> (Int, Text) -> [String]
renderSourceLine grouped (lineNum, srcLine) =
  let anns = maybe [] (sortOn annotationStartCol) (Map.lookup lineNum grouped)
   in T.unpack srcLine : renderAnnotations anns

-- | Get the start column (1-indexed) from an annotation.
annotationStartCol :: ResolutionAnnotation -> Int
annotationStartCol ann =
  case resolutionSpan ann of
    SourceSpan _ _ startCol _ _ _ _ -> startCol
    NoSourceSpan -> maxBound

-- | Render the annotation label: "(namespace) target"
annotationLabel :: ResolutionAnnotation -> String
annotationLabel ann =
  renderResolutionNamespace (resolutionNamespace ann)
    <> " "
    <> renderResolvedName (resolutionTarget ann)

-- | Render annotation lines for a group of annotations on the same source line.
--
-- Annotations are placed right-to-left. The rightmost annotation is placed
-- first. Then each subsequent annotation (going left) is checked: if it fits
-- on the current annotation line without overlapping, it's added to that line.
-- Otherwise, vertical bars are drawn for pending annotations and a new
-- annotation line is started.
renderAnnotations :: [ResolutionAnnotation] -> [String]
renderAnnotations [] = []
renderAnnotations anns =
  -- Sort by column, then process right-to-left
  let sorted = sortOn annotationStartCol anns
      -- Each annotation becomes (0-indexed column, label text)
      items = [(annotationStartCol a - 1, annotationLabel a) | a <- sorted]
   in layoutAnnotationLines items

-- | A placed annotation: (0-indexed column, label text)
type AnnotationItem = (Int, String)

-- | Layout annotations into lines, combining where possible.
--
-- Items are sorted left-to-right by column. We process them right-to-left,
-- greedily packing onto the current line if they fit.
layoutAnnotationLines :: [AnnotationItem] -> [String]
layoutAnnotationLines [] = []
layoutAnnotationLines items =
  -- Process right-to-left: the rightmost annotation always goes on the
  -- current line. Then try to add each annotation to the left.
  let reversed = reverse items
      (currentLine, deferred) = packLine reversed
   in renderAnnotationLine items currentLine deferred
        : layoutAnnotationLines deferred

-- | Try to pack annotations onto one line, processing right-to-left.
-- Returns (annotations that fit on this line, annotations deferred to next lines).
-- Both lists are in left-to-right order (by column).
packLine :: [AnnotationItem] -> ([AnnotationItem], [AnnotationItem])
packLine [] = ([], [])
packLine (rightmost : rest) =
  let go _minCol [] fitted deferred = (fitted, deferred)
      go minCol (item@(col, label) : remaining) fitted deferred =
        -- The annotation marker is "└─ " (3 chars) followed by the label.
        -- It occupies columns [col .. col + 3 + length label - 1].
        -- To fit, the end of this annotation must be < minCol (with a gap).
        let annotEnd = col + 3 + length label
         in if annotEnd <= minCol
              then go col remaining (item : fitted) deferred
              else go minCol remaining fitted (item : deferred)
   in go (fst rightmost) rest [rightmost] []

-- | Render a single annotation line, given the full set of items (for drawing
-- vertical bars for deferred annotations) and the items placed on this line.
renderAnnotationLine :: [AnnotationItem] -> [AnnotationItem] -> [AnnotationItem] -> String
renderAnnotationLine _allItems placedOnLine deferredItems =
  -- Build the line character by character.
  -- For deferred items, draw │ at their column.
  -- For placed items, draw └─ label at their column.
  let deferredCols = map fst deferredItems
      -- Build from left to right
      buildLine :: Int -> [AnnotationItem] -> [Int] -> String
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

-- | Render annotated source for all modules in a resolve result.
--
-- Takes the original source texts (one per module, in the same order they were
-- passed to 'resolve') and the resolve result. Returns a list of annotated
-- source strings, one per module, sorted by module name.
renderAnnotatedResolveResult :: [Text] -> ResolveResult -> [String]
renderAnnotatedResolveResult sources result =
  let modules = resolvedModules result
      -- Build (moduleName, sourceText) pairs from the resolved modules and sources
      moduleSourcePairs = zipWith (\src modu -> (moduleDisplayName modu, src)) sources modules
      -- Collect annotations from the AST and merge with extra annotations
      allAnnotations = mergeModules (collectModules modules <> resolvedAnnotations result)
      annotationMap = Map.fromList allAnnotations
   in -- For each module (sorted by name), render annotated source
      map
        ( \(name, src) ->
            renderAnnotatedSource src (Map.findWithDefault [] name annotationMap)
        )
        (sortOn fst moduleSourcePairs)
