module Cursor.Brick.List where

import Brick.Types
import Cursor.List

listCursorWidgetM ::
  ([a] -> [a] -> m (Widget n)) -> ListCursor a -> m (Widget n)
listCursorWidgetM = foldListCursor

listCursorWidget :: ([a] -> [a] -> Widget n) -> ListCursor a -> Widget n
listCursorWidget = foldListCursor
