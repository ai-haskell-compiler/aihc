-- | Generic-ish (not really) pretty-printing utilities and abstractions for rendering the table of cells.
module ParkBench.Internal.Pretty
  ( -- * High-level row/cell machinery
    R (..),
    Cellular (..),
    BytesCell (..),
    BytesPerSecondCell (..),
    IncomparablePercentageCell (..),
    IncomparableWord3Cell (..),
    NanosecondsCell (..),
    NumberCell (..),
    NumberCell' (..),
    PercentageCell (..),
    PercentageCell' (..),
    rowMaker,
    Table (..),
    renderTable,
    RowGroup (..),
    Row (..),
    Cell (..),
    isEmptyCell,
    Color (..),
  )
where

import qualified Data.Array as Array
import Data.Foldable (toList)
import Data.Maybe
import Data.Ord (Down (..))
import Data.String (IsString (..))
import qualified Data.Text as Text
import ParkBench.Internal.Array1 (Array1)
import qualified ParkBench.Internal.Array1 as Array1
import ParkBench.Internal.Builder (Builder)
import qualified ParkBench.Internal.Builder as Builder
import ParkBench.Internal.Prelude
import qualified ParkBench.Internal.Terminal as Terminal

------------------------------------------------------------------------------------------------------------------------
-- High-level row/cell machinery

data R a b
  = R Cell (a -> Maybe b)

class Ord a => Cellular a where
  cellDelta :: a -> a -> Double
  cellString :: a -> Builder

newtype BytesCell
  = BytesCell Double
  deriving (Eq) via Double
  deriving (Ord) via Down Double

instance Cellular BytesCell where
  cellDelta = coerce doubleDelta
  cellString (BytesCell r) = Builder.bytes4 r

newtype BytesPerSecondCell
  = BytesPerSecondCell Double
  deriving (Eq) via Double
  deriving (Ord) via Down Double

instance Cellular BytesPerSecondCell where
  cellDelta = coerce doubleDelta
  cellString (BytesPerSecondCell r) = prettyBytesPerSecond r

prettyBytesPerSecond :: Double -> Builder
prettyBytesPerSecond r =
  if Builder.null s
    then Builder.empty
    else s <> "/s"
  where
    s = Builder.bytes4 r

newtype IncomparablePercentageCell
  = IncomparablePercentageCell Double
  deriving (Eq) via Double
  deriving (Ord) via Down Double

instance Cellular IncomparablePercentageCell where
  cellDelta _ _ = 0
  cellString (IncomparablePercentageCell r) = Builder.percentage r

newtype NumberCell
  = NumberCell Double
  deriving (Eq) via Double
  deriving (Ord) via Down Double

instance Cellular NumberCell where
  cellDelta = coerce doubleDelta
  cellString (NumberCell r) = Builder.double4 r

newtype NumberCell'
  = NumberCell' Double
  deriving (Eq, Ord) via Double
  deriving (Cellular) via NumberCell

newtype NanosecondsCell
  = NanosecondsCell Double
  deriving (Eq) via Double
  deriving (Ord) via Down Double

instance Cellular NanosecondsCell where
  cellDelta = coerce doubleDelta
  cellString (NanosecondsCell r) = Builder.nanos4 r

newtype PercentageCell
  = PercentageCell Double
  deriving (Eq) via Double
  deriving (Ord) via Down Double

instance Cellular PercentageCell where
  cellDelta = coerce doubleDelta
  cellString (PercentageCell r) = Builder.percentage r

prettyDelta :: Double -> Builder
prettyDelta n
  | n >= 1 = Builder.double4 (n + 1) <> "x"
  | n <= -0.5 = Builder.double4 (-1 `divideDouble` (n + 1)) <> "x"
  | otherwise = Builder.percentage n

newtype PercentageCell'
  = PercentageCell' Double
  deriving (Eq, Ord) via Double
  deriving (Cellular) via PercentageCell

newtype IncomparableWord3Cell
  = IncomparableWord3Cell Word64
  deriving (Eq) via Word64
  deriving (Ord) via Down Word64

instance Cellular IncomparableWord3Cell where
  cellDelta _ _ = 0 -- incomparable
  cellString (IncomparableWord3Cell r) = Builder.word3 r

doubleDelta :: Double -> Double -> Double
doubleDelta v1 v2 =
  (v2 - v1) `divideDouble` v1

rowMaker :: forall a. Array1 a -> (forall b. Cellular b => R a b -> Row)
rowMaker (Array1.uncons -> (summary0, summaries0)) (R name (f :: a -> Maybe b)) =
  let cols =
        maybe
          EmptyCell
          (Cell White . Builder.build . cellString)
          (f summary0) :
        makeCols (f summary0) (toList summaries0)
   in if all isEmptyCell cols then EmptyRow else Row (name : cols)
  where
    makeCols :: Maybe b -> [a] -> [Cell]
    makeCols s0 = \case
      [] -> totalDelta
      s1 : ss ->
        case (s0, f s1) of
          (Nothing, Just v1) -> EmptyCell : Cell White (Builder.build (cellString v1)) : makeCols (Just v1) ss
          (Just v0, Just v1) -> deltaCell v0 v1 : Cell White (Builder.build (cellString v1)) : makeCols (Just v1) ss
          (_, Nothing) -> EmptyCell : EmptyCell : makeCols Nothing ss

    totalDelta :: [Cell]
    totalDelta =
      if n < 2
        then []
        else case (f summary0, f (summaries0 Array.! (n - 1))) of
          (Just v0, Just v1) -> [deltaCell v0 v1]
          _ -> [EmptyCell]
      where
        n = length summaries0

deltaCell :: Cellular a => a -> a -> Cell
deltaCell v1 v2 =
  if Builder.null (cellString v1) || Builder.null (cellString v2)
    then EmptyCell
    else Cell color (Builder.build (prettyDelta (cellDelta v1 v2)))
  where
    color :: Color
    color =
      case compare v1 v2 of
        LT -> Green
        EQ -> White
        GT -> Red

data Table
  = Table ![Cell] ![RowGroup]
  deriving stock (Show)

data RowGroup
  = RowGroup {-# UNPACK #-} !Text ![Row]
  deriving stock (Show)

data Row
  = -- | Invariant: 1+ cells; not all cells are empty
    Row ![Cell]
  | EmptyRow
  deriving stock (Show)

data Cell
  = EmptyCell
  | Cell !Color {-# UNPACK #-} !Text
  deriving stock (Show)

cellBuilder :: Cell -> Builder
cellBuilder = \case
  EmptyCell -> Builder.empty
  Cell color (Builder.text -> s) ->
    case color of
      Blue -> Terminal.blue s
      Green -> Terminal.green s
      Red -> Terminal.red s
      White -> s

cellWidth :: Cell -> Int
cellWidth = \case
  Cell _ s -> Text.length s
  EmptyCell -> 0

instance IsString Cell where
  fromString =
    Cell White . Text.pack

data Color
  = Blue
  | Green
  | Red
  | White
  deriving stock (Show)

isEmptyCell :: Cell -> Bool
isEmptyCell = \case
  -- don't yet have invariant that s is non-null
  Cell _ s -> Text.null s
  EmptyCell -> True

data Align
  = AlignLeft
  | AlignRight

renderTable :: Table -> Builder
renderTable (Table labels rowGroups) =
  (header : mapMaybe renderRowGroup rowGroups ++ [footer]) `Builder.sepBy` Builder.char '\n'
  where
    header :: Builder
    header =
      Builder.char '┌'
        <> ((map (renderCell AlignLeft '─') (zip (map (+ 2) widths) labels)) `Builder.sepBy` Builder.char '┬')
        <> Builder.char '┐'

    footer :: Builder
    footer =
      Builder.char '└'
        <> ((map (\n -> Builder.chars (n + 2) '─') widths) `Builder.sepBy` Builder.char '┴')
        <> Builder.char '┘'

    renderRowGroup :: RowGroup -> Maybe Builder
    renderRowGroup (RowGroup label rows) =
      case mapMaybe renderRow rows of
        [] -> Nothing
        s -> Just ((line : s) `Builder.sepBy` Builder.char '\n')
      where
        line =
          case widths of
            w0 : ws ->
              Builder.char '├'
                <> "\ESC[1m\ESC[97m"
                <> Builder.text (Text.map dash label)
                <> "\ESC[39m\ESC[22m"
                <> Builder.chars (w0 + 2 - Text.length label) '─'
                <> foldMap (\n -> Builder.char '┼' <> Builder.chars (n + 2) '─') ws
                <> Builder.char '┤'
            _ -> undefined

        dash :: Char -> Char
        dash = \case
          ' ' -> '─'
          c -> c

    renderRow :: Row -> Maybe Builder
    renderRow = \case
      Row row -> Just ("│ " <> ((map (renderCell AlignRight ' ') (zip widths row)) `Builder.sepBy` " │ ") <> " │")
      EmptyRow -> Nothing

    renderCell :: Align -> Char -> (Int, Cell) -> Builder
    renderCell align bg (n, cell) =
      case align of
        AlignLeft -> cellBuilder cell <> padding
        AlignRight -> padding <> cellBuilder cell
      where
        padding = Builder.chars (n - cellWidth cell) bg

    widths :: [Int]
    widths =
      foldl' step0 (map (subtract 1 . cellWidth) labels) rowGroups
      where
        step0 :: [Int] -> RowGroup -> [Int]
        step0 acc (RowGroup label rows) =
          foldl' (step1 label) acc rows

        step1 :: Text -> [Int] -> Row -> [Int]
        step1 label acc = \case
          Row [] -> error "empty row"
          Row (col : cols) -> zipWith max (max (Text.length label) (cellWidth col) : map cellWidth cols) acc
          EmptyRow -> acc
