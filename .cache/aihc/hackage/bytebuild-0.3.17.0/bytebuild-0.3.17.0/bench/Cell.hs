{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Cell
  ( Cell (..)
  , cells
  ) where

import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word32)

-- A cell in a CSV file
data Cell
  = CellString !ShortText
  | CellNumber !Word32

-- Some sample data to encode as a CSV
cells :: SmallArray (SmallArray Cell)
cells =
  [ [CellString "Randy", CellString "Gutiérrez", CellNumber 41, CellNumber 343]
  , [CellString "Édith", CellString "Piaf", CellNumber 63, CellNumber 453]
  , [CellString "Martha", CellString "Washington", CellNumber 51, CellNumber 634]
  , [CellString "Julius", CellString "Caesar", CellNumber 1, CellNumber 6922]
  , [CellString "Robert", CellString "Redford", CellNumber 24, CellNumber 617]
  , [CellString "Violet", CellString "Crawley", CellNumber 71, CellNumber 150]
  , [CellString "Lázaro", CellString "Cárdenas", CellNumber 58, CellNumber 299]
  , [CellString "Anastasia", CellString "San Martin", CellNumber 103, CellNumber 3214]
  , [CellString "Mad", CellString "Max", CellNumber 37, CellNumber 918]
  , [CellString "Sidonie-Gabrielle", CellString "Collette", CellNumber 25, CellNumber 904]
  ]
