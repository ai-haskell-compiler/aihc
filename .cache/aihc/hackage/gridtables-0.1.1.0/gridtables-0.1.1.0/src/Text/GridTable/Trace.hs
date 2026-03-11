{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{- |
Module      : Text.GridTable.Trace
Copyright   : © 2022-2023 Posit Software, PBC
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Trace cells of a grid table.
-}

module Text.GridTable.Trace
  ( traceLines
  , TraceInfo (..)
  , initialTraceInfo
  , tableFromTraceInfo
  ) where

import Prelude hiding (lines)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Function (on)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Text.DocLayout (charWidth, realLength)
import Text.GridTable.ArrayTable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Traces out the cells in the given lines and converts them to a
-- table containing the bare cell lines.
traceLines :: [Text] -> Maybe (ArrayTable [Text])
traceLines lines =
  let charGrid = toCharGrid lines
      specs1   = colSpecsInLine '-' charGrid 1
      partSeps = findSeparators charGrid
      -- The first separator can never be a part separator line (with
      -- =), but it can contain column alignment markers, so it is
      -- always normalized it as well.
      charGrid' = convertToNormalLines (1:map partSepLine partSeps) charGrid
      traceInfo = traceCharGrid charGrid' initialTraceInfo
  in if Set.null (gridCells traceInfo)
     then fail "no cells"
     else return $ tableFromTraceInfo traceInfo partSeps specs1

-- | Type used to represent the 2D layout of table characters
type CharGrid = Array (CharRow, CharCol) GChar

-- | Index of a half-width character in the character-wise
-- representation.
type CharIndex = (CharRow, CharCol)

-- | Character row
newtype CharRow = CharRow Int
  deriving stock (Eq, Show)
  deriving newtype (Enum, Ix, Num, Ord)

-- | Character column
newtype CharCol = CharCol { fromCharCol :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Enum, Ix, Num, Ord)

data GChar
  = C Char           -- ^ half- or full-width character
  | CZ [Char] Char   -- ^ character preceded by zero-width chars
  | WP               -- ^ padding for wide characters
  | Missing          -- ^ padding for short lines
  deriving stock (Eq)

-- | Info on the grid. Used to keep track of information collected while
-- tracing a character grid. The set of cells is used as a kind of queue
-- during parsing, while the other data is required to assemble the
-- final table.
data TraceInfo = TraceInfo
  { gridRowSeps  :: Set CharRow
  , gridColSeps  :: Set CharCol
  , gridCorners  :: Set CharIndex
  , gridCells    :: Set CellTrace
  }

-- | Initial tracing info.
initialTraceInfo :: TraceInfo
initialTraceInfo = TraceInfo
  { gridRowSeps  = Set.fromList [CharRow 1]
  , gridColSeps  = Set.fromList [CharCol 1]
  , gridCorners  = Set.fromList [(CharRow 1, CharCol 1)]
  , gridCells    = Set.empty
  }

-- | Converts a list of lines into a char array.
toCharGrid :: [Text] -> CharGrid
toCharGrid lines =
  let chars = foldr (\t m -> max m (realLength t)) 0 lines -- potential overcount
      gbounds = ( (CharRow 1, CharCol 1)
                , (CharRow (length lines), CharCol chars)
                )
      toGChars []     = repeat Missing
      toGChars (c:cs) = case charWidth c of
        2 -> C c : WP : toGChars cs
        1 -> C c : toGChars cs
        _ -> case span ((== 0) . charWidth) cs of
               (zw, [])     -> [CZ (c:zw) '\0']
               (zw, c':cs') -> CZ (c:zw) c' :
                               case charWidth c' of
                                 2 -> WP : toGChars cs'
                                 _ -> toGChars cs'
      extendedLines = map (take chars . toGChars . T.unpack) lines
  in listArray gbounds (mconcat extendedLines)

-- | Information on, and extracted from, a body separator line. This is a line
-- that uses @=@ instead of @-@ to demark cell borders.
data PartSeparator = PartSeparator
  { partSepLine    :: CharRow
  , partSepColSpec :: [ColSpec]
  }

-- | Alignment and character grid position of a column.
data ColSpec = ColSpec
  { colStart :: CharCol
  , colEnd   :: CharCol
  , colAlign :: Alignment
  }

-- | Finds the row indices of all separator lines, i.e., lines that
-- contain only @+@ and @=@ characters.
findSeparators :: CharGrid -> [PartSeparator]
findSeparators charGrid = foldr go [] rowIdxs
  where
    gbounds = bounds charGrid
    rowIdxs = [fst (fst gbounds) .. fst (snd gbounds)]
    go i seps = case colSpecsInLine '=' charGrid i of
                  Nothing -> seps
                  Just colspecs -> PartSeparator i colspecs : seps

-- | Checks for a separator in the given line, returning the column properties
-- if it finds such a line.
colSpecsInLine :: Char  -- ^ Character used in line (usually @-@)
               -> CharGrid -> CharRow -> Maybe [ColSpec]
colSpecsInLine c charGrid i =
  case charGrid ! (i, firstCol) of
    C '+' -> loop [] (firstCol + 1)
    _     -> Nothing
  where
    loop acc j = case colSpecAt j of
                   Nothing      -> Nothing
                   Just Nothing -> Just $ reverse acc
                   Just (Just colspec) ->
                     loop (colspec:acc) (colEnd colspec + 1)
    gbounds = bounds charGrid
    firstCol = snd (fst gbounds)
    lastCol = snd (snd gbounds)
    colSpecAt :: CharCol -> Maybe (Maybe ColSpec)
    colSpecAt j
      | j >= lastCol = Just Nothing
      | otherwise = case findEnd (j + 1) of
          Nothing               -> Nothing
          Just (end, rightMark) ->
            let leftMark = charGrid ! (i, j) == C ':'
                align = case (leftMark, rightMark) of
                  (False , False) -> AlignDefault
                  (True  , False) -> AlignLeft
                  (False , True ) -> AlignRight
                  (True  , True ) -> AlignCenter
                colspec = ColSpec
                  { colStart = j
                  , colEnd = end
                  , colAlign = align
                  }
            in pure (pure colspec)
    findEnd j = case charGrid ! (i, j) of
      C '+' -> pure (j, False)
      C ':' -> if charGrid ! (i, j + 1) == C '+'
                  then pure (j + 1, True)
                  else Nothing
      C c'
        | c' == c -> findEnd (j + 1)
      _           -> Nothing

-- | Returns new character grid in which the given lines have been
-- converted to normal cell-separating lines.
convertToNormalLines :: [CharRow] -> CharGrid -> CharGrid
convertToNormalLines sepLines charGrid = runSTArray $ do
  mutGrid <- thaw charGrid
  let gbounds = bounds charGrid
      cols = [snd (fst gbounds) .. snd (snd gbounds)]
  forM_ sepLines $ \rowidx -> do
    forM_ cols $ \colidx -> do
      let idx = (rowidx, colidx)
      c <- readArray mutGrid idx
      -- convert `=` to `-` and remove alignment markers
      case c of
        C '=' -> writeArray mutGrid idx (C '-')
        C ':' -> writeArray mutGrid idx (C '-')
        _        -> pure ()
  return mutGrid

-- | Trace the given char grid and collect all relevant info.
-- This function calls itself recursively.
traceCharGrid :: CharGrid
              -> TraceInfo
              -> TraceInfo
traceCharGrid charGrid traceInfo =
  -- Get the next corner an remove it from the set of unparsed corners.
  case Set.minView (gridCorners traceInfo) of
    Nothing -> traceInfo
    Just (startIdx@(top, left), corners) ->
      case traceCell charGrid startIdx of
        Nothing ->
          -- Corner is not a top-left corner of another cell. Continue
          -- with the remaining corners.
          traceCharGrid charGrid traceInfo { gridCorners = corners }
        Just ((bottom, right), newrowseps, newcolseps) -> do
          let content = getLines charGrid startIdx (bottom, right)
          let cell = CellTrace content left right top bottom
          let rowseps = gridRowSeps traceInfo
          let colseps = gridColSeps traceInfo
          let cells   = gridCells traceInfo
          traceCharGrid charGrid $ TraceInfo
            { gridRowSeps = newrowseps `Set.union` rowseps
            , gridColSeps = newcolseps `Set.union` colseps
            , gridCorners = Set.insert (top, right) $
                            Set.insert (bottom, left) corners
            , gridCells = cell `Set.insert` cells
            }

type ScanResult = (CharIndex, Set CharRow, Set CharCol)

type RowSeps = Set CharRow
type ColSeps = Set CharCol

-- | Traces a single cell on the grid, starting at the given position.
traceCell :: CharGrid -> CharIndex -> Maybe ScanResult
traceCell = scanRight

-- | Scans right from the given index, following a cell separator line
-- to the next column marker (@+@), then scans down. Returns the
-- bottom-right index of the cell if it can complete the trace, and
-- nothing if it reaches the end of line before the trace is complete.
--
-- All row and column markers found during scanning are seen are
-- collected and returned as part of the result.
scanRight :: CharGrid -> CharIndex -> Maybe ScanResult
scanRight charGrid start@(top, left) = do
  loop Set.empty (left + 1)
  where
    loop :: ColSeps -> CharCol -> Maybe ScanResult
    loop colseps j
      | not (bounds charGrid `inRange` (top, j)) = Nothing
      | otherwise = case charGrid ! (top, j) of
          C '-' -> loop colseps (j + 1)
          C '+' ->
            let colseps' = Set.insert j colseps
            in case scanDown charGrid start j of
                 Nothing -> loop colseps' (j + 1) <|>
                            lastCellInRow charGrid start (j + 1)
                 Just (end, rowseps, newcolseps) -> pure
                   ( end
                   , rowseps
                   , colseps' `Set.union` newcolseps
                   )
          _ -> Nothing

-- | Like 'scanRight', but scans down in the given column.
scanDown :: CharGrid
         -> CharIndex  -- ^ top-left corner of cell
         -> CharCol    -- ^ column of the cell's right border
         -> Maybe ScanResult
scanDown charGrid start@(top, _left) right = do
  loop Set.empty (top + 1)
  where
    loop :: RowSeps -> CharRow -> Maybe ScanResult
    loop rowseps i =
      if not (bounds charGrid `inRange` (i, right))
      then Nothing
      else case charGrid ! (i, right) of
             C '+' ->
               let rowseps' = Set.insert i rowseps
               in case scanLeft charGrid start (i, right) of
                    Nothing -> loop rowseps' (i + 1)
                    Just (newrowseps, colseps) ->
                      Just ( (i, right)
                           , rowseps' `Set.union` newrowseps
                           , colseps
                           )
             C '|' -> loop rowseps (i + 1)
             _ -> -- all but the final column must be terminated
               if right == snd (snd (bounds charGrid))
               then loop rowseps (i + 1)
               else Nothing

-- | Like 'scanRight', but scans left starting at the bottom-right
-- corner.
scanLeft :: CharGrid -> CharIndex -> CharIndex
         -> Maybe (RowSeps, ColSeps)
scanLeft charGrid start@(_top,left) end@(bottom, right) =
  let  go :: CharCol -> Maybe ColSeps -> Maybe ColSeps
       go _ Nothing = Nothing
       go j (Just colseps) = case charGrid ! (bottom, j) of
                               C '+' -> Just (Set.insert j colseps)
                               C '-' -> Just colseps
                               _     -> Nothing

  in if charGrid ! (bottom, left) /= C '+'
     then Nothing
     else
       case foldr go (Just Set.empty) [(right - 1), right - 2 .. (left + 1)] of
         Nothing      -> Nothing
         Just colseps ->
           case scanUp charGrid start end of
             Just rowseps -> Just (rowseps, colseps)
             Nothing      -> Nothing

-- | Scans up from the bottom-left corner back to the top-left corner.
scanUp :: CharGrid -> CharIndex -> CharIndex
       -> Maybe RowSeps
scanUp charGrid (top, left) (bottom, _right) =
  let go :: CharRow -> Maybe RowSeps -> Maybe RowSeps
      go _ Nothing = Nothing
      go i (Just rowseps) = case charGrid ! (i, left) of
                              C '+' -> Just (Set.insert i rowseps)
                              C '|' -> Just rowseps
                              _     -> Nothing
  in foldr go (Just Set.empty) [bottom - 1, bottom - 2 .. top + 1]

lastCellInRow :: CharGrid -> CharIndex -> CharCol -> Maybe ScanResult
lastCellInRow charGrid start@(top, _left) right =
  if bounds charGrid `inRange` (top, right) &&
     charGrid ! (top, right) == Missing
  then scanRestOfLines charGrid start
  else Nothing

lastColumn :: CharGrid -> CharCol
lastColumn = snd . snd . bounds

lastRow :: CharGrid -> CharRow
lastRow = fst . snd . bounds

-- | Forgiving version of scanRight, allowing misaligned line ends
scanRightRestOfLine :: CharGrid -> CharIndex -> CharRow -> Maybe ColSeps
scanRightRestOfLine charGrid (_top, left) bottom =
  let  go :: CharCol -> Maybe ColSeps -> Maybe ColSeps
       go _ Nothing = Nothing
       go j (Just colseps) = case charGrid ! (bottom, j) of
                               C '+'   -> Just (Set.insert j colseps)
                               C '-'   -> Just colseps
                               Missing -> Just colseps
                               _       -> Nothing

  in if charGrid ! (bottom, left) /= C '+'
     then Nothing
     else foldr go (Just Set.empty) [left + 1 .. lastColumn charGrid]

scanRestOfLines :: CharGrid -> CharIndex -> Maybe ScanResult
scanRestOfLines charGrid start@(top, _left) =
  let  go :: Maybe CharIndex -> CharRow -> Maybe CharIndex
       go idx i = idx <|>
                  case scanRightRestOfLine charGrid start i of
                    Nothing -> Nothing
                    Just _colseps -> Just (i, lastColumn charGrid)
  in case foldl' go Nothing [top + 1 .. lastRow charGrid] of
       Just (bottom, right) -> Just
                               ( (bottom, right)
                               , Set.singleton bottom
                               , Set.singleton right)
       Nothing          -> Nothing

-- | Gets the textual contents, i.e. the lines of a cell.
getLines :: CharGrid -> CharIndex -> CharIndex -> [Text]
getLines charGrid (top, left) (bottom, right) =
  let rowIdxs = [top + 1 .. bottom - 1]
      colIdxs = [left + 1 .. right - 1]
      toChars rowIdx colIdx = case charGrid ! (rowIdx, colIdx) of
        C c     -> [c]
        CZ zw c -> zw ++ [c]
        _       -> []
  in map (\ir -> T.pack $ concatMap (toChars ir) colIdxs)
         rowIdxs

-- | Traced cell with raw contents and border positions.
data CellTrace = CellTrace
  { cellTraceContent :: [Text]
  , cellTraceLeft    :: CharCol
  , cellTraceRight   :: CharCol
  , cellTraceTop     :: CharRow
  , cellTraceBottom  :: CharRow
  }
  deriving stock (Eq, Show)

instance Ord CellTrace where
  x `compare` y =
    case (compare `on` cellTraceTop) x y of
      EQ -> (compare `on` cellTraceLeft) x y
      o  -> o

-- | Create a final grid table from line scanning data.
tableFromTraceInfo :: TraceInfo
                   -> [PartSeparator]
                   -> Maybe [ColSpec]
                   -> ArrayTable [Text]
tableFromTraceInfo traceInfo partSeps colSpecsFirstLine =
  let rowseps = Set.toAscList $ gridRowSeps traceInfo
      colseps = Set.toAscList $ gridColSeps traceInfo
      rowindex = Map.fromList $ zip rowseps [1..]
      colindex = Map.fromList $ zip colseps [1..]
      colwidths = [ b - a - 1 | (b, a) <- zip (tail colseps) colseps ]
      colSpecs = zip
                 (map colAlign
                   (case partSeps of
                       partSep:_ -> partSepColSpec partSep
                       []        -> fromMaybe [] colSpecsFirstLine)
                   ++ repeat AlignDefault)
                 (map fromCharCol colwidths)
      lastCol = ColIndex (length colwidths)
      mlastLine = Set.lookupMax (gridRowSeps traceInfo)
      tableHead = case partSeps of
        ps:_   -> subtract 1 <$> partSepLine ps `Map.lookup` rowindex
        []     -> Nothing
      tableFoot = case reverse partSeps of
        rps:rps':_ | Just (partSepLine rps) == mlastLine ->
          partSepLine rps' `Map.lookup` rowindex
        _ ->
          Nothing
  in ArrayTable
     { arrayTableCells = runSTArray (toMutableArray traceInfo rowindex colindex)
     , arrayTableHead = tableHead
     , arrayTableFoot = tableFoot
     , arrayTableColSpecs = listArray (1, lastCol) colSpecs
     }

-- | Create a mutable cell array from the scanning data.
toMutableArray :: TraceInfo
               -> Map.Map CharRow RowIndex
               -> Map.Map CharCol ColIndex
               -> ST s (STArray s CellIndex (GridCell [Text]))
toMutableArray traceInfo rowindex colindex = do
  let nrows = Map.size rowindex - 1
  let ncols = Map.size colindex - 1
  let gbounds = ( (RowIndex 1, ColIndex 1)
                , (RowIndex nrows, ColIndex ncols)
                )
  tblgrid <- newArray gbounds FreeCell
  forM_ (Set.toAscList $ gridCells traceInfo) $
    \(CellTrace content left right top bottom) -> do
      let cellPos = do
            rnum <- Map.lookup top rowindex
            cnum <- Map.lookup left colindex
            rs   <- RowSpan . fromRowIndex . subtract rnum <$>
                    Map.lookup bottom rowindex
            cs   <- ColSpan . fromColIndex . subtract cnum <$>
                    Map.lookup right colindex
            pure ((rnum, cnum), rs, cs)
      let (idx, rowspan, colspan) = case cellPos of
            Just cp -> cp
            Nothing -> error "A cell or row index was not found"
      writeArray tblgrid idx . FilledCell $
        ContentCell rowspan colspan content
      forM_ (continuationIndices idx rowspan colspan) $ \contIdx -> do
        -- FIXME: ensure that the cell has not been filled yet
        writeArray tblgrid contIdx $
          FilledCell (ContinuationCell idx)
      -- Swap BuilderCells with normal GridCells.
  let fromBuilderCell :: BuilderCell -> GridCell [Text]
      fromBuilderCell = \case
        FilledCell c -> c
        FreeCell     ->
          -- Found an unassigned cell; replace with empty cell. TODO: This
          -- should be reported as a warning.
          ContentCell 1 1 mempty
  mapArray fromBuilderCell tblgrid

-- | Calculate the array indices that are spanned by a cell.
continuationIndices :: (RowIndex, ColIndex)
                    -> RowSpan -> ColSpan
                    -> [CellIndex]
continuationIndices (RowIndex ridx, ColIndex cidx) rowspan colspan =
  let (RowSpan rs) = rowspan
      (ColSpan cs) = colspan
  in [ (RowIndex r, ColIndex c) | r <- [ridx..(ridx + rs - 1)]
                                , c <- [cidx..(cidx + cs - 1)]
                                , (r, c) /= (ridx, cidx)]

-- | Helper type used to track which indices have been already been
-- filled in a mutable cell array.
data BuilderCell
  = FilledCell (GridCell [Text])
  | FreeCell
