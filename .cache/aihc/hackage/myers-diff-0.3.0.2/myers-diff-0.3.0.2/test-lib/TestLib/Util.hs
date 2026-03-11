
module TestLib.Util (
  mkDelete
  , mkInsert
  ) where

import Data.Diff.Types
import Data.Text as T


mkDelete :: (Int, Int) -> (Int, Int) -> ChangeEvent
mkDelete (l1, c1) (l2, c2) = ChangeEvent (Range (Position l1 c1) (Position l2 c2)) ""

mkInsert :: (Int, Int) -> (Int, Int) -> Text -> ChangeEvent
mkInsert (l1, c1) (l2, c2) t = ChangeEvent (Range (Position l1 c1) (Position l2 c2)) t
