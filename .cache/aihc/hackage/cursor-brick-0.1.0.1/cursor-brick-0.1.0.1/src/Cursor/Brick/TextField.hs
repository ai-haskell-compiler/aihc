module Cursor.Brick.TextField where

import Brick.Types as Brick
import Brick.Widgets.Core as Brick
import Cursor.Brick.Text
import Cursor.List.NonEmpty
import Cursor.TextField

-- | Make a textfield cursor widget with a blink-y box.
--
-- This function does not wrap lines.
-- Otherwise, because of the way indexes work, there would be rendering errors for text that crosses the side of the terminal.
selectedTextFieldCursorWidget :: n -> TextFieldCursor -> Widget n
selectedTextFieldCursorWidget n (TextFieldCursor tfc) =
  flip foldNonEmptyCursor tfc $ \befores current afters ->
    vBox $
      concat
        [ map textWidget befores,
          [selectedTextCursorWidget n current],
          map textWidget afters
        ]

-- | Make a textfield cursor widget without a blink-y box.
--
-- This function does not wrap lines .
textFieldCursorWidget :: TextFieldCursor -> Widget n
textFieldCursorWidget (TextFieldCursor tfc) =
  flip foldNonEmptyCursor tfc $ \befores current afters ->
    vBox $
      concat
        [ map textWidget befores,
          [textCursorWidget current],
          map textWidget afters
        ]
