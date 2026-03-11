module Termbox.Internal.Image
  ( Image (..),
    char,
    at,
    atRow,
    atCol,
    fg,
    bg,
    bold,
    underline,
    blink,
  )
where

import qualified Data.Char as Char
import Foreign.C (CInt (..), CWchar (..))
import Termbox.Bindings.Hs (tb_change_cell)
import Termbox.Internal.Color (Color)
import Termbox.Internal.Pos (Pos (..))
import Termbox.Internal.Style (Style)
import qualified Termbox.Internal.Style as Style

-- | An image.
--
-- * Create an image with 'char'.
-- * Set an image's color with 'fg' \/ 'bg'.
-- * Style an image with 'bold' \/ 'underline' \/ 'blink'.
-- * Translate an image with 'at' \/ 'atRow' \/ 'atCol'.
-- * Overlay an image atop another with @(<>)@.
newtype Image
  = Image (Pos -> Style -> IO ())

instance Monoid Image where
  mempty = Image mempty

instance Semigroup Image where
  Image f <> Image g =
    Image \pos style -> do
      f pos style
      g pos style

-- | Create an image from a character.
--
-- If the character is not 1 character wide, it will not be displayed.
char :: Char -> Image
char ch =
  Image \Pos {row, col} style ->
    tb_change_cell
      col
      row
      (if wcwidth (charToCWchar ch) == 1 then ch else ' ')
      (Style.asForeground style)
      (Style.asBackground style)

-- | Translate an image.
at :: Pos -> Image -> Image
at offset (Image draw) =
  Image \pos -> draw (pos <> offset)

-- | Translate an image by a number of rows.
atRow :: Int -> Image -> Image
atRow row =
  at (Pos row 0)

-- | Translate an image by a number of columns.
atCol :: Int -> Image -> Image
atCol col =
  at (Pos 0 col)

styled :: Style -> Image -> Image
styled overrides (Image draw) =
  Image \pos style -> draw pos (overrides <> style)

-- | Set the foreground color of an image.
fg :: Color -> Image -> Image
fg =
  styled . Style.fg

-- | Set the background color of an image.
bg :: Color -> Image -> Image
bg =
  styled . Style.bg

-- | Make an image bold.
bold :: Image -> Image
bold =
  styled Style.bold

-- | Make an image underlined.
underline :: Image -> Image
underline =
  styled Style.underline

-- | Make an image blink.
blink :: Image -> Image
blink =
  styled Style.blink

charToCWchar :: Char -> CWchar
charToCWchar =
  fromIntegral @Int @CWchar . Char.ord

foreign import capi unsafe "wchar.h wcwidth"
  wcwidth :: CWchar -> CInt
