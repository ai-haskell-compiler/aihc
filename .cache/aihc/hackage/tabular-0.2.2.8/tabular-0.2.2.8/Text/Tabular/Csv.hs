module Text.Tabular.Csv where

import Data.List (intersperse, transpose)
import Text.CSV (printCSV)
import Text.Tabular

-- | for simplicity, we assume that each cell is rendered
--   on a single line
render :: (rh -> String)
       -> (ch -> String)
       -> (a -> String)
       -> Table rh ch a
       -> String
render fr fc f (Table rh ch cells) =
  printCSV $ chStrings : cells2
 where
  -- cell2 includes the row and column labels
  cells2 = zipWith (\h cs -> h : map f cs) rhStrings cells
  chStrings = "" : (map fc $ headerContents ch)
  rhStrings = map fr $ headerContents rh
