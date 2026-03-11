{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{- |
Module      : Text.GridTable
Copyright   : © 2022-2023 Posit Software, PBC
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Parse reStructuredText-style grid tables.
-}

module Text.GridTable
  ( module Text.GridTable.ArrayTable
    -- * Parse from character stream
  , gridTable
    -- * List-based representation
  , Cell (..)
  , rows
  ) where

import Prelude hiding (lines)
import Data.Array (Array, elems, bounds)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Text.GridTable.ArrayTable
import Text.GridTable.Parse (gridTable)

colBounds :: Array CellIndex a -> (ColIndex, ColIndex)
colBounds = bimap snd snd . bounds

-- | Returns the rows of a grid table as lists of simple cells.
rows :: ArrayTable a -> [[Cell a]]
rows gt =
  let tarr = arrayTableCells gt
      ncols = fromColIndex . uncurry (flip (-)) $ colBounds tarr
      toSimpleCell = \case
        ContentCell rs cs c -> Just $ Cell c rs cs
        ContinuationCell {} -> Nothing
      mkRows :: [[Cell a]] -> [GridCell a] -> [[Cell a]]
      mkRows rs = \case
        [] -> reverse rs
        xs -> let (r, xs') = splitAt (ncols + 1) xs
              in mkRows (mapMaybe toSimpleCell r:rs) xs'
  in mkRows [] $ elems tarr

-- | Raw grid table cell
data Cell a = Cell
  { cellContent :: a
  , cellRowSpan :: RowSpan
  , cellColSpan :: ColSpan
  }
  deriving stock (Eq, Ord, Show)
