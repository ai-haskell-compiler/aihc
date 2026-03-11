-- | Typically to use Rainbox you will want these @import@s:
--
-- @
-- import qualified Data.Sequence as Seq
-- import Rainbow
-- import Rainbox
--
-- -- and, for GHC before 7.10:
-- import Data.Monoid
-- @
--
-- Rainbox does not re-export anything from "Data.Sequence" or
-- "Rainbow" because I don't know if you want all those things dumped
-- into the same namespace.
--
-- "Rainbox.Tutorial" wil get you started.  "Rainbox.Core" contains
-- the implementation details, which you should not need to pay
-- attention to (if you do need to use "Rainbox.Core" for ordinary
-- usage of the library, that's a bug; please report it.)
module Rainbox
  ( -- * Alignment and Boxes
    Alignment
  , Horizontal
  , Vertical
  , center
  , left
  , right
  , top
  , bottom
  , centerH
  , centerV
  , Box
  , Orientation ( spacer, spreader )

  -- * Box construction
  , fromChunk
  , blank
  , wrap

  -- * Rendering
  , render
  , putBox
  , hPutBox

  -- * Tables
  --
  -- | Types and functions to build a simple spreadsheet-like grid.
  -- You create a nested 'Seq' of 'Cell', and then use 'tableByRows'
  -- or 'tableByColumns' to create a 'Box', which you can then
  -- render using "Rainbow" functions.  Each column is as wide as
  -- necessary to accomodate the widest cell in the column, but no
  -- wider, which means the columns will tend to meld together.  To
  -- add separators you'll have to add separator cells in yourself.

  -- ** Cell type
  , Cell(..)
  , separator

  -- ** van Laarhoven lenses
  , rows
  , horizontal
  , vertical
  , background

  -- ** Table builders
  , tableByRows
  , tableByColumns

  -- * Bi-color tables
  --
  -- | Build tables with rows of alternating background colors.
  , BicolorTableCellLine
  , BicolorTableCell
  , BicolorTableRow
  , BicolorTable(..)
  , bicolorTableToBox
  , hPutBicolorTable
  , putBicolorTable

  -- ** van Laarhoven lenses
  , bctEvenBackground
  , bctOddBackground
  , bctSpacerWidth
  , bctAlignments
  , bctRows

  ) where

import Rainbox.Core
import Rainbox.BicolorTable
