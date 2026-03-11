{-# LANGUAGE LambdaCase #-}

-- A variant of CSV encoding that does not perform
-- any escaping or quoting. This is in its own module
-- to make it easy to analyze the GHC Core that it
-- gets compiled to.
module SimpleCsv
  ( encodeRows
  ) where

import Cell (Cell (..))
import Data.Primitive (SmallArray)

import qualified Data.Bytes.Builder as B
import qualified Data.Foldable as F

encodeRows :: SmallArray (SmallArray Cell) -> B.Builder
encodeRows =
  F.foldr
    (\r x -> encodeSimpleCsvRow r (B.ascii '\n' <> x))
    mempty

encodeSimpleCsvRow :: SmallArray Cell -> B.Builder -> B.Builder
encodeSimpleCsvRow cs b =
  F.foldr
    (\c x -> encodeSimpleCsvCell c <> B.ascii ',' <> x)
    b
    cs

encodeSimpleCsvCell :: Cell -> B.Builder
encodeSimpleCsvCell = \case
  CellNumber n -> B.word32Dec n
  CellString t -> B.shortTextUtf8 t
