-- | Render a source text with labels under it. A golden renderer finds
-- the labels and their spans, and this module puts each label below the
-- start of its span.
module Aihc.Testing.AnnotatedModule
  ( renderLabeledSource,
  )
where

import Aihc.Parser.Syntax
  ( SourceSpan,
    sourceSpanStartCol,
    sourceSpanStartLine,
  )
import Data.List (intercalate, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as T
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)

-- | Render a source text with labels that the caller placed. Each label
-- goes below the start of its span.
renderLabeledSource :: T.Text -> [(SourceSpan, Doc ann)] -> String
renderLabeledSource source placed =
  length labels `seq` renderAnnotatedSource (T.unpack source) labels
  where
    labels = [PlacedLabel span' (renderLabelDoc doc) | (span', doc) <- placed]

data PlacedLabel = PlacedLabel
  { placedSpan :: !SourceSpan,
    placedText :: !String
  }
  deriving (Eq, Show)

renderLabelDoc :: Doc ann -> String
renderLabelDoc doc =
  let label = renderDoc doc
   in if '\n' `elem` label
        then error ("renderLabeledSource: multiline annotation label: " <> show label)
        else label

renderDoc :: Doc ann -> String
renderDoc =
  renderString . layoutPretty defaultLayoutOptions

renderAnnotatedSource :: String -> [PlacedLabel] -> String
renderAnnotatedSource source labels =
  let grouped = groupByLine labels
   in intercalate "\n" (concatMap (renderSourceLine grouped) (zip [1 ..] (lines source)))

groupByLine :: [PlacedLabel] -> Map.Map Int [PlacedLabel]
groupByLine = foldr insertLabel Map.empty
  where
    insertLabel label =
      Map.insertWith (<>) (sourceSpanStartLine (placedSpan label)) [label]

renderSourceLine :: Map.Map Int [PlacedLabel] -> (Int, String) -> [String]
renderSourceLine grouped (lineNum, srcLine) =
  let labels = maybe [] (sortOn labelKey) (Map.lookup lineNum grouped)
   in srcLine : renderAnnotationLines labels

labelKey :: PlacedLabel -> (Int, String)
labelKey label =
  (labelStartCol label, placedText label)

labelStartCol :: PlacedLabel -> Int
labelStartCol = sourceSpanStartCol . placedSpan

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
