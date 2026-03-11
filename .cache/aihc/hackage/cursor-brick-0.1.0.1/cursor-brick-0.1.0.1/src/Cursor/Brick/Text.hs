{-# LANGUAGE OverloadedStrings #-}

module Cursor.Brick.Text where

import Brick.Types as Brick
import Brick.Widgets.Core as Brick
import Cursor.Text
import Data.Text (Text)
import qualified Data.Text as T

-- | Make a text cursor widget with a blink-y box.
--
-- This function does not wrap the given text.
--
-- Otherwise, because of the way indexes work, there would be rendering errors for text that crosses the side of the terminal.
selectedTextCursorWidget :: n -> TextCursor -> Widget n
selectedTextCursorWidget n tc =
  Brick.showCursor n (Brick.Location (textCursorIndex tc, 0)) $
    textCursorWidget tc

-- | Make a text cursor widget without a blink-y box.
--
-- This function does not wrap the given text.
textCursorWidget :: TextCursor -> Widget n
textCursorWidget tc =
  txt $
    let t = sanitiseText $ rebuildTextCursor tc
     in if T.null t
          then " "
          else t

-- | Draw an arbitrary Text, it will be sanitised.
textWidget :: Text -> Widget n
textWidget = txt . nonNullLinesText . sanitiseText

-- | Draw an arbitrary Text (with wrapping), it will be sanitised.
textWidgetWrap :: Text -> Widget n
textWidgetWrap = txtWrap . nonNullLinesText . sanitiseText

-- | Draw an arbitrary single-line Text, it will be sanitised.
textLineWidget :: Text -> Widget n
textLineWidget = txt . nonNullText . sanitiseText

-- | Draw an arbitrary single-line Text (with wrapping), it will be sanitised.
textLineWidgetWrap :: Text -> Widget n
textLineWidgetWrap = txtWrap . nonNullText . sanitiseText

-- | Makes every line of a Text non-empty using `nonNullText`
nonNullLinesText :: Text -> Text
nonNullLinesText = T.intercalate "\n" . map nonNullText . T.splitOn "\n"

-- | Makes a text non-empty.
--
-- This turns the empty text into " " and leaves other text as-is.
nonNullText :: Text -> Text
nonNullText "" = " "
nonNullText t = t

-- | Replace tabs by spaces so that brick doesn't render nonsense.
--
-- See https://hackage.haskell.org/package/brick/docs/Brick-Widgets-Core.html#v:txt
sanitiseText :: Text -> Text
sanitiseText =
  T.map $ \c ->
    case c of
      '\t' -> ' '
      _ -> c
