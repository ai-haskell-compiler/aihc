module Termbox.Internal.Color
  ( -- * Color
    Color (..),
    defaultColor,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bright,
    color,
    gray,

    -- * MaybeColor
    MaybeColor,
    unMaybeColor,
    nothingColor,
    justColor,
  )
where

import Data.Coerce (coerce)
import Data.Word (Word16)
import Termbox.Bindings.Hs

-- | A color.
--
-- There are three classes of colors:
--
-- * Basic named colors and their bright variants, such as 'red' and 'bright' 'blue'.
-- * Miscellaneous colors, such as @'color' 33@.
-- * Monochrome colors that range from black (@'gray' 0@) to white (@'gray' 23@).
newtype Color
  = Color Tb_attrs
  deriving newtype (Eq)

-- These intentionally don't use e.g. TB_RED, 'cause they're all off-by-one in TB_OUTPUT_256

defaultColor :: Color
defaultColor =
  Color (Tb_attrs 0)

red :: Color
red =
  Color (Tb_attrs 1)

green :: Color
green =
  Color (Tb_attrs 2)

yellow :: Color
yellow =
  Color (Tb_attrs 3)

blue :: Color
blue =
  Color (Tb_attrs 4)

magenta :: Color
magenta =
  Color (Tb_attrs 5)

cyan :: Color
cyan =
  Color (Tb_attrs 6)

white :: Color
white =
  Color (Tb_attrs 7)

-- | Make a basic color bright.
bright :: Color -> Color
bright =
  coerce bright_

bright_ :: Word16 -> Word16
bright_ c
  | c <= 7 = c + 8
  | otherwise = c

-- | A miscellaneous color.
--
-- Valid values are in the range @[0, 215]@; values outside of this range are clamped.
color :: Int -> Color
color =
  coerce (fromIntegral @Int @Word16 . (+ 16) . max 0 . min 215)

-- | A monochrome color; black is 0 and white is 23.
--
-- Valid values are in the range @[0, 23]@; values outside of this range are clamped.
gray :: Int -> Color
gray =
  coerce (fromIntegral @Int @Word16 . (+ 232) . max 0 . min 23)

-- This is a more efficient `Maybe Color`; we represent Nothing by WORD_MAX (which isn't a valid termbox color)
newtype MaybeColor
  = MaybeColor Color
  deriving stock (Eq)

unMaybeColor :: MaybeColor -> Tb_attrs
unMaybeColor c
  | c == nothingColor = _TB_DEFAULT
  | otherwise = coerce @_ @Tb_attrs c

nothingColor :: MaybeColor
nothingColor =
  coerce @Word16 maxBound

justColor :: Color -> MaybeColor
justColor =
  MaybeColor
