{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
-- lens-simple makeLenses will not make type signatures
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | Contains the innards of 'Rainbox'.  You shouldn't need anything
-- in here.  Some functions here are partial or have undefined results
-- if their inputs don't respect particular invariants.
module Rainbox.Core where

import           Control.Monad (join)
import qualified Data.Foldable as F
import           Data.Function ((&))
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Data.Traversable as T
import           Control.Lens (Lens', lens)
import           Rainbow ( Chunk , Radiant , chunk , back, hPutChunks)
import           Rainbow.Types (Chunk (_yarn))
import           System.IO

-- # Alignment

-- | Alignment.  Used in conjunction with 'Horizontal' and 'Vertical',
-- this determines how a payload aligns with the axis of a 'Box'.
data Alignment a = Center | NonCenter a
  deriving (Eq, Ord, Show, Functor, F.Foldable, T.Traversable)

instance Semigroup (Alignment a) where
  x <> y = case x of
    Center -> y
    NonCenter a -> case y of
      Center -> NonCenter a
      NonCenter b -> NonCenter b

-- | 'mempty' is 'center'.  'mappend' takes the rightmost non-'center'
-- value.

instance Monoid (Alignment a) where
  mempty = Center

-- # Horizontal and vertical

-- | Determines how a payload aligns with a horizontal axis.
data Horizontal = Top | Bottom
  deriving (Eq, Ord, Show)

-- | Determines how a payload aligns with a vertical axis.
data Vertical = Port | Starboard
  deriving (Eq, Ord, Show)

-- | Place this payload so that it is centered on the vertical axis or
-- horizontal axis.
center :: Alignment a
center = Center

-- | Center horizontally; like 'center', but monomorphic.
centerH :: Alignment Horizontal
centerH = center

-- | Center vertically; like 'center', but monomorphic.
centerV :: Alignment Vertical
centerV = center

-- | Place this payload's left edge on the vertical axis.
left :: Alignment Vertical
left = NonCenter Port

-- | Place this payload's right edge on the vertical axis.
right :: Alignment Vertical
right = NonCenter Starboard

-- | Place this payload's top edge on the horizontal axis.
top :: Alignment Horizontal
top = NonCenter Top

-- | Place this payload's bottom edge on the horizontal axis.
bottom :: Alignment Horizontal
bottom = NonCenter Bottom


-- # Width and height

-- | A count of rows.
newtype Height = Height Int
  deriving (Eq, Ord, Show)

-- | A count of columns.
newtype Width = Width Int
  deriving (Eq, Ord, Show)

class HasHeight a where
  height :: a -> Int

instance HasHeight Height where
  height (Height a) = max 0 a

instance HasHeight Chunk where
  height _ = 1

instance (HasHeight a, HasHeight b) => HasHeight (Either a b) where
  height = either height height

class HasWidth a where
  width :: a -> Int

instance HasWidth Width where
  width (Width a) = max 0 a

instance HasWidth Chunk where
  width ck = X.length . _yarn $ ck

instance (HasWidth a, HasWidth b) => HasWidth (Either a b) where
  width = either width width

-- # Core

-- | A 'Core' is either a single 'Chunk' or, if the box is blank, is
-- merely a height and a width.
newtype Core = Core (Either Chunk (Height, Width))
  deriving (Eq, Ord, Show)

instance HasWidth Core where
  width (Core ei) = either width (width . snd) ei

instance HasHeight Core where
  height (Core ei) = either height (height . fst) ei

-- # Rods

-- | An intermediate type used in rendering; it consists either of
-- text 'Chunk' or of a number of spaces coupled with a background color.
newtype Rod = Rod (Either (Int, Radiant) Chunk)
  deriving (Eq, Ord, Show)

instance HasWidth Rod where
  width (Rod ei) = case ei of
    Left (i, _) -> max 0 i
    Right c -> width c

-- # RodRows

-- | A list of screen rows; each screen row is a 'Seq' of 'Rod'.
--
-- A 'RodRows' with width but no height does nothing if rendered
-- alone, but it can affect the width of other 'RodRows' if combined
-- with them.
data RodRows
  = RodRowsWithHeight (Seq (Seq Rod))
  -- ^ Each outer 'Seq' represents a single screen row.  Each 'Seq'
  -- has a height of 1.
  --
  -- The outer 'Seq' must have a length of at least 1, even if the
  -- inner 'Seq' is empty.  If the outer 'Seq' has a length of zero,
  -- undefined behavior occurs.  For a 'RodRows' with no height and no
  -- width, use 'RodRowsNoHeight'.

  | RodRowsNoHeight Int
  -- ^ A 'RodRows' that has no height.  If the 'Int' is less than 1,
  -- the 'RodRows' has no width and no height.  Otherwise, the
  -- 'RodRows' has no height but has the given width.
  deriving (Eq, Ord, Show)

instance HasHeight RodRows where
  height (RodRowsWithHeight sq) = Seq.length sq
  height (RodRowsNoHeight _) = 0

instance HasWidth RodRows where
  width (RodRowsWithHeight sq) = F.foldl' max 0 . fmap (F.sum . fmap width) $ sq
  width (RodRowsNoHeight i) = max 0 i

-- | Convert a 'Core' to a 'Seq' of 'Rod' for rendering.
rodRowsFromCore :: Radiant -> Core -> RodRows
rodRowsFromCore bk (Core ei) = case ei of
  Left ck -> RodRowsWithHeight . Seq.singleton
    . Seq.singleton . Rod . Right $ ck
  Right (Height h, Width w)
    | h < 1  -> RodRowsNoHeight w
    | otherwise -> RodRowsWithHeight . Seq.replicate h . Seq.singleton
        . Rod . Left $ (w, bk)

-- | Converts a 'RodRows' to a nested 'Seq' of 'Chunk' in
-- preparation for rendering.  Newlines are added to the end of each
-- line.
chunksFromRodRows :: RodRows -> Seq (Seq Chunk)
chunksFromRodRows rr = case rr of
  RodRowsWithHeight sq -> fmap (|> chunk "\n") . fmap (fmap chunkFromRod) $ sq
    where
      chunkFromRod (Rod ei) = case ei of
        Left (i, r) -> (chunk . X.replicate i $ " ") & back r
        Right c -> c
  RodRowsNoHeight _ -> Seq.empty


-- # Payload

-- | A 'Payload' holds a 'RodRows', which determines the number
-- and content of the screen rows.  The 'Payload' also has an
-- 'Alignment', which specifies how the payload aligns with the axis.
-- Whether the 'Alignment' is 'Horizontal' or 'Vertical' determines
-- the orientation of the 'Payload'.  The 'Payload' also contains a
-- background color, which is type 'Radiant'.  The background color
-- extends continuously from the 'Payload' in both directions that are
-- perpendicular to the axis.

data Payload a = Payload (Alignment a) Radiant (Either RodRows Core)
  deriving (Eq, Ord, Show)

instance HasWidth (Payload a) where
  width (Payload _ _ ei) = width ei

instance HasHeight (Payload a) where
  height (Payload _ _ ei) = height ei

-- # Padding and merging

-- | Adds padding to the top and bottom of each Payload.  A Payload
-- with a Core is converted to a RodRows and has padding added; a
-- Payload with a RodRows has necessary padding added to the top and
-- bottom.  The number of elements in the resulting Seq is the same as
-- the number of elements in the input Seq; no merging is performed.

addVerticalPadding
  :: Box Horizontal
  -> Seq RodRows
addVerticalPadding bx@(Box sqnce) = fmap eqlize sqnce
  where
    maxTop = above bx
    maxBot = below bx
    eqlize bhp@(Payload _ rd ei) = case ei of
      Left rr -> eqlzeRodRows rr
      Right cre -> eqlzeRodRows (rodRowsFromCore rd cre)
      where
        eqlzeRodRows rr = case rr of
          RodRowsWithHeight sq -> RodRowsWithHeight $ tp w <> sq <> bot w
          RodRowsNoHeight i
            | maxTop + maxBot == 0 -> RodRowsNoHeight i
            | otherwise -> RodRowsWithHeight $ tp w <> bot w
          where
            w = width rr
        tp w = Seq.replicate (max 0 (maxTop - above bhp)) (pad w)
        bot w = Seq.replicate (max 0 (maxBot - below bhp)) (pad w)
        pad w = Seq.singleton . Rod . Left $ (w, rd)

-- | Merges multiple horizontal RodRows into a single RodRows.  All
-- RodRows must already have been the same height; if they are not the
-- same height, undefined behavior occurs.

horizontalMerge :: Seq RodRows -> RodRows
horizontalMerge sqn = case viewl sqn of
  EmptyL -> RodRowsNoHeight 0
  x :< xs -> case x of
    RodRowsNoHeight i -> RodRowsNoHeight $ F.foldl' comb i xs
      where
        comb acc x' = case x' of
          RodRowsNoHeight i' -> acc + i'
          RodRowsWithHeight _ -> error "horizontalMerge: error 1"
    RodRowsWithHeight sq -> RodRowsWithHeight $ F.foldl' comb sq xs
      where
        comb acc rr = case rr of
          RodRowsWithHeight sq' -> Seq.zipWith (<>) acc sq'
          RodRowsNoHeight _ -> error "horizontalMerge: error 2"

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2

-- | Adds padding to the left and right of each Payload.
-- A Payload with a Core is converted to a RodRows and has padding
-- added; a Payload with a RodRows has necessary padding added to the
-- left and right.  The number of elements in the resulting Seq is
-- the same as the number of elements in the input Seq; no merging is
-- performed.

addHorizontalPadding
  :: Box Vertical
  -> Seq RodRows
addHorizontalPadding bx@(Box sqnce) = fmap eqlize sqnce
  where
    maxLeft = port bx
    maxRight = starboard bx
    eqlize (Payload a rd ei) = case ei of
      Left rr -> addLeftRight rr
      Right cre -> addLeftRight $ rodRowsFromCore rd cre
      where
        addLeftRight (RodRowsNoHeight _) = RodRowsNoHeight $ maxLeft + maxRight
        addLeftRight (RodRowsWithHeight sq) = RodRowsWithHeight $
          fmap addLeftRightToLine sq
        addLeftRightToLine lin = padder lenLft <> lin <> padder lenRgt
          where
            lenLin = F.sum . fmap width $ lin
            lenLft = case a of
              Center -> maxLeft - (fst . split $ lenLin)
              NonCenter Port -> maxLeft
              NonCenter Starboard -> maxLeft - lenLin
            lenRgt = case a of
              Center -> maxRight - (snd . split $ lenLin)
              NonCenter Port -> maxRight - lenLin
              NonCenter Starboard -> maxRight
            padder len
              | len < 1 = Seq.empty
              | otherwise = Seq.singleton . Rod . Left $ (len, rd)


-- | Merge multiple vertical RodRows into a single RodRows.  Each
-- RodRows should already be the same width.

verticalMerge :: Seq RodRows -> RodRows
verticalMerge sqnce = case viewl sqnce of
  EmptyL -> RodRowsNoHeight 0
  x :< xs -> F.foldl' comb x xs
    where
      comb acc rr = case (acc, rr) of
        (RodRowsNoHeight w, RodRowsNoHeight _) -> RodRowsNoHeight w
        (RodRowsNoHeight _, RodRowsWithHeight sq) -> RodRowsWithHeight sq
        (RodRowsWithHeight sq, RodRowsNoHeight _) -> RodRowsWithHeight sq
        (RodRowsWithHeight sq1, RodRowsWithHeight sq2) ->
          RodRowsWithHeight $ sq1 <> sq2

-- # Box

-- | A 'Box' is the central building block.  It consists of zero or
-- more payloads; each payload has the same orientation, which is either
-- 'Horizontal' or 'Vertical'.  This orientation also determines
-- the orientation of the entire 'Box'.
--
-- A 'Box' is a 'Monoid' so you can combine them using the usual
-- monoid functions.  For a 'Box' 'Vertical', the leftmost values
-- added with 'mappend' are at the top of the 'Box'; for a 'Box'
-- 'Horizontal', the leftmost values added with 'mappend' are on the
-- left side of the 'Box'.
newtype Box a = Box (Seq (Payload a))
  deriving (Eq, Ord, Show)

instance Semigroup (Box a) where
  (Box x) <> (Box y) = Box (x <> y)

instance Monoid (Box a) where
  mempty = Box Seq.empty

-- # Orientation

-- | This typeclass is responsible for transforming a 'Box' into
-- Rainbow 'Chunk' so they can be printed to your screen.  This
-- requires adding appropriate whitespace with the right colors, as
-- well as adding newlines in the right places.
class Orientation a where
  rodRows :: Box a -> RodRows

  spacer :: Radiant -> Int -> Box a
  -- ^ Builds a one-dimensional box of the given size; its single
  -- dimension is parallel to the axis.  When added to a
  -- box, it will insert blank space of the given length.  For a 'Box'
  -- 'Horizontal', this produces a horizontal line; for a 'Box'
  -- 'Vertical', a vertical line.

  spreader :: Alignment a -> Int -> Box a
  -- ^ Builds a one-dimensional box of the given size; its single
  -- dimension is perpendicular to the axis.  This can be used to make
  -- a 'Box' 'Vertical' wider or a 'Box' 'Horizontal' taller.

instance Orientation Vertical where
  rodRows = verticalMerge . addHorizontalPadding

  spacer r i = Box . Seq.singleton $
    Payload (NonCenter Port) r (Right . Core . Right $
      (Height (max 0 i), Width 0))
  spreader a i = Box . Seq.singleton $
    Payload a mempty (Right . Core . Right $
      (Height 0, Width (max 0 i)))

instance Orientation Horizontal where
  rodRows = horizontalMerge . addVerticalPadding

  spacer r i = Box . Seq.singleton $
    Payload (NonCenter Top) r (Right . Core . Right $
      (Height 0, Width (max 0 i)))
  spreader a i = Box . Seq.singleton $
    Payload a mempty (Right . Core . Right $
      (Height (max 0 i), Width 0))

-- # port, starboard, above, below


-- | Things that are oriented around a vertical axis.
class LeftRight a where
  -- | Length to the left of the vertical axis.
  port :: a -> Int

  -- | Length to the right of the vertical axis.
  starboard :: a -> Int

-- | Things that are oriented around a horizontal axis.
class UpDown a where
  -- | Number of lines above the horizontal axis.
  above :: a -> Int
  -- | Number of lines below the horizontal axis.
  below :: a -> Int


instance LeftRight (Payload Vertical) where
  port (Payload a _ ei) = case a of
    NonCenter Port -> 0
    NonCenter Starboard -> width ei
    Center -> fst . split . width $ ei

  starboard (Payload a _ s3) = case a of
    NonCenter Port -> width s3
    NonCenter Starboard -> 0
    Center -> snd . split . width $ s3

instance UpDown (Payload Horizontal) where
  above (Payload a _ s3) = case a of
    NonCenter Top -> 0
    NonCenter Bottom -> height s3
    Center -> fst . split . height $ s3

  below (Payload a _ s3) = case a of
    NonCenter Top -> height s3
    NonCenter Bottom -> 0
    Center -> snd . split . height $ s3

instance LeftRight (Box Vertical) where
  port (Box sq) = F.foldl' max 0 . fmap port $ sq
  starboard (Box sq) = F.foldl' max 0 . fmap starboard $ sq

instance HasWidth (Box Vertical) where
  width b = port b + starboard b

instance HasHeight (Box Vertical) where
  height (Box sq) = F.sum . fmap height $ sq

instance UpDown (Box Horizontal) where
  above (Box sq) = F.foldl' max 0 . fmap above $ sq
  below (Box sq) = F.foldl' max 0 . fmap below $ sq

instance HasHeight (Box Horizontal) where
  height b = above b + below b

instance HasWidth (Box Horizontal) where
  width (Box sq) = F.sum . fmap width $ sq

-- # Box construction

-- | Construct a box from a single 'Chunk'.
fromChunk
  :: Alignment a
  -> Radiant
  -- ^ Background color.  The background color in the 'Chunk' is not
  -- changed; this background is used if the 'Payload' must be padded
  -- later on.
  -> Chunk
  -> Box a
fromChunk a r = Box . Seq.singleton . Payload a r  . Right . Core . Left

-- | Construct a blank box.  Useful for adding in background spacers.
-- For functions that build one-dimensional boxes, see 'spacer' and
-- 'spreader'.
blank
  :: Alignment a
  -> Radiant
  -- ^ Color for the blank area.
  -> Height
  -> Width
  -> Box a
blank a r h w =
  Box . Seq.singleton . Payload a r . Right . Core . Right $ (h, w)

-- | Wrap a 'Box' in another 'Box'.  Useful for changing a
-- 'Horizontal' 'Box' to a 'Vertical' one, or simply for putting a
-- 'Box' inside another one to control size and background color.
wrap
  :: Orientation a
  => Alignment b
  -- ^ Alignment for new 'Box'.  This also determines whether the new
  -- 'Box' is 'Horizontal' or 'Vertical'.
  -> Radiant
  -- ^ Background color for new box
  -> Box a
  -> Box b
wrap a r = Box . Seq.singleton . Payload a r . Left . rodRows

-- # Box rendering

-- | Convert a box to a 'Seq' of 'Chunk' in preparation for rendering.
-- Use 'F.toList' to convert the 'Seq' of 'Chunk' to a list so that
-- you can print it using the functions in "Rainbow".
render :: Orientation a => Box a -> Seq Chunk
render = join . chunksFromRodRows . rodRows

-- | Renders a 'Box' to the given 'Handle'.  This uses 'hPutChunks' so consult
-- that function for more details on how this works; generally it is going to
-- use the maximum number of colors possible for your terminal.
hPutBox :: Orientation a => Handle -> Box a -> IO ()
hPutBox h b = hPutChunks h (F.toList . render $ b)

-- | Uses 'hPutBox' to render the given 'Box' to standard output.
putBox :: Orientation a => Box a -> IO ()
putBox = hPutBox stdout


-- # Tables

-- | A single cell in a spreadsheet-like grid.
data Cell = Cell
  { _rows :: Seq (Seq Chunk)
  -- ^ The cell can have multiple rows of text; there is one 'Seq' for
  -- each row of text.
  , _horizontal :: Alignment Horizontal
  -- ^ How this 'Cell' should align compared to other 'Cell' in its
  -- row.
  , _vertical :: Alignment Vertical
  -- ^ How this 'Cell' should align compared to other 'Cell' in its column.
  , _background :: Radiant
  -- ^ Background color for this cell.  The background in the
  -- individual 'Chunk' in the 'cellRows' are not affected by
  -- 'cellBackground'; instead, 'cellBackground' determines the color
  -- of necessary padding that will be added so that the cells make a
  -- uniform table.
  } deriving (Eq, Ord, Show)

rows :: Lens' Cell (Seq (Seq Chunk))
rows = lens _rows (\cel fld -> cel { _rows = fld })

horizontal :: Lens' Cell (Alignment Horizontal)
horizontal = lens _horizontal (\cel fld -> cel { _horizontal = fld })

vertical :: Lens' Cell (Alignment Vertical)
vertical = lens _vertical (\cel fld -> cel { _vertical = fld })

background :: Lens' Cell Radiant
background = lens _background (\cel fld -> cel { _background = fld })

instance Semigroup Cell where
  (Cell rx hx vx bx) <> (Cell ry hy vy by)
    = Cell (zipSeqs rx ry) (hx <> hy) (vx <> vy) (bx <> by)
    where
      zipSeqs x y = Seq.zipWith (<>) x' y'
        where
          x' = x <> Seq.replicate
            (max 0 (Seq.length y - Seq.length x)) Seq.empty
          y' = y <> Seq.replicate
            (max 0 (Seq.length x - Seq.length y)) Seq.empty


-- | 'mappend' combines two 'Cell' horizontally so they are
-- side-by-side, left-to-right.  The '_horizontal', '_vertical', and
-- '_background' fields are combined using their respective 'Monoid'
-- instances.  'mempty' uses the respective 'mempty' value for each
-- field.
instance Monoid Cell where
  mempty = Cell mempty mempty mempty mempty

-- | Creates a blank 'Cell' with the given background color and width;
-- useful for adding separators between columns.
separator :: Radiant -> Int -> Cell
separator rd i = Cell (Seq.singleton (Seq.singleton ck)) top left rd
  where
    ck = (chunk $ X.replicate (max 0 i) " ") & back rd

-- Cells by row:
-- 0. Ensure each row is equal length
-- 1. Create one BoxV for each cell
-- 2. Create widest cell map
-- 3. Pad each BoxV to appropriate width, using cellVert alignment
-- 4. Convert each BoxV to BoxH, using cellHoriz and cellBackground
-- 5. mconcatSeq each row
-- 6. Convert each row to BoxV; use default background
--    and center alignment
-- 7. mconcatSeq the rows

-- | Create a table where each inner 'Seq' is a row of cells,
-- from left to right.  If necessary, blank cells are added to the end
-- of a row to ensure that each row has the same number of cells as
-- the longest row.
tableByRows :: Seq (Seq Cell) -> Box Vertical
tableByRows
  = mconcatSeq
  . fmap rowToBoxV
  . fmap mconcatSeq
  . fmap (fmap toBoxH)
  . uncurry padBoxV
  . addWidthMap
  . fmap (fmap cellToBoxV)
  . equalize mempty

rowToBoxV :: Box Horizontal -> Box Vertical
rowToBoxV = wrap center mempty

cellToBoxV :: Cell -> (Box Vertical, Alignment Horizontal, Radiant)
cellToBoxV (Cell rs ah av rd) = (bx, ah, rd)
  where
    bx = mconcatSeq
       . fmap (wrap av rd)
       . fmap (mconcatSeq . fmap (fromChunk top rd))
       $ rs

toBoxH
  :: (Box Vertical, Alignment Horizontal, Radiant)
  -> Box Horizontal
toBoxH (bv, ah, rd) = wrap ah rd bv

addWidthMap
  :: Seq (Seq (Box Vertical, b, c))
  -> (M.Map Int (Int, Int), Seq (Seq (Box Vertical, b, c)))
addWidthMap sqnce = (m, sqnce)
  where
    m = widestCellMap . fmap (fmap (\(a, _, _) -> a)) $ sqnce

padBoxV
  :: M.Map Int (Int, Int)
  -> Seq (Seq (Box Vertical, a, b))
  -> Seq (Seq (Box Vertical, a, b))
padBoxV mp = fmap (Seq.mapWithIndex f)
  where
    f idx (bx, a, b) = (bx <> padLeft <> padRight, a, b)
      where
        (lenL, lenR) = mp M.! idx
        padLeft = spreader right lenL
        padRight = spreader left lenR


widestCellMap :: Seq (Seq (Box Vertical)) -> M.Map Int (Int, Int)
widestCellMap = F.foldl' outer M.empty
  where
    outer mpOuter = Seq.foldlWithIndex inner mpOuter
      where
        inner mpInner idx bx = case M.lookup idx mpInner of
          Nothing -> M.insert idx (port bx, starboard bx) mpInner
          Just (pOld, sOld) -> M.insert idx
            (max pOld (port bx), max sOld (starboard bx)) mpInner

-- Table by columns:
--
-- 0.  Equalize columns
-- 1.  Create one BoxH for each cell
-- 2.  Create tallest cell map
-- 3.  Pad each BoxH to appropriate height, using cellHeight alignment
-- 4.  Convert each BoxH to BoxV, using cellVert and cellBackground
-- 5.  mconcatSeq each column
-- 6.  Convert each column to BoxH
-- 7.  mconcatSeq the columns

-- | Create a table where each inner 'Seq' is a column of cells,
-- from top to bottom.  If necessary, blank cells are added to the end
-- of a column to ensure that each column has the same number of cells
-- as the longest column.
tableByColumns :: Seq (Seq Cell) -> Box Horizontal
tableByColumns
  = mconcatSeq
  . fmap rowToBoxH
  . fmap mconcatSeq
  . fmap (fmap toBoxV)
  . uncurry padBoxH
  . addHeightMap
  . fmap (fmap cellToBoxH)
  . equalize mempty


rowToBoxH :: Box Vertical -> Box Horizontal
rowToBoxH = wrap top mempty


cellToBoxH :: Cell -> (Box Horizontal, Alignment Vertical, Radiant)
cellToBoxH (Cell rs ah av rd) = (bx, av, rd)
  where
    bx = wrap ah rd
       . mconcatSeq
       . fmap (wrap av rd)
       . fmap (mconcatSeq . fmap (fromChunk top rd))
       $ rs

addHeightMap
  :: Seq (Seq (Box Horizontal, b, c))
  -> (M.Map Int (Int, Int), Seq (Seq (Box Horizontal, b, c)))
addHeightMap sqnce = (m, sqnce)
  where
    m = tallestCellMap . fmap (fmap (\(a, _, _) -> a)) $ sqnce

tallestCellMap :: Seq (Seq (Box Horizontal)) -> M.Map Int (Int, Int)
tallestCellMap = F.foldl' outer M.empty
  where
    outer mpOuter = Seq.foldlWithIndex inner mpOuter
      where
        inner mpInner idx bx = case M.lookup idx mpInner of
          Nothing -> M.insert idx (above bx, below bx) mpInner
          Just (aOld, bOld) -> M.insert idx
            (max aOld (above bx), max bOld (below bx)) mpInner


padBoxH
  :: M.Map Int (Int, Int)
  -> Seq (Seq (Box Horizontal, a, b))
  -> Seq (Seq (Box Horizontal, a, b))
padBoxH mp = fmap (Seq.mapWithIndex f)
  where
    f idx (bx, a, b) = (bx <> padTop <> padBot, a, b)
      where
        (lenT, lenB) = mp M.! idx
        padTop = spreader bottom lenT
        padBot = spreader top lenB


toBoxV
  :: (Box Horizontal, Alignment Vertical, Radiant)
  -> Box Vertical
toBoxV (bh, av, rd) = wrap av rd bh


-- | Ensures that each inner 'Seq' is the same length by adding the
-- given empty element where needed.
equalize :: a -> Seq (Seq a) -> Seq (Seq a)
equalize emp sqnce = fmap adder sqnce
  where
    maxLen = F.foldl' max 0 . fmap Seq.length $ sqnce
    adder sq = sq <> pad
      where
        pad = Seq.replicate (max 0 (maxLen - Seq.length sq)) emp

mconcatSeq :: Monoid a => Seq a -> a
mconcatSeq = F.foldl' (<>) mempty

