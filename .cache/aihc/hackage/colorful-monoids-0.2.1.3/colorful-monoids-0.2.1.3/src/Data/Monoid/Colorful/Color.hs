{-# LANGUAGE DeriveGeneric #-}

module Data.Monoid.Colorful.Color (
  Color(..)
  , Style(..)
  , reduceColor
) where

import Data.Word (Word8)
import Data.Bool (bool)
import Data.Monoid.Colorful.Term
import GHC.Generics (Generic)

-- | Rendering style
data Style
  = Bold       -- ^ Bold font
  | Italic     -- ^ Italic font
  | Underline  -- ^ Underlined text
  | Invert     -- ^ Invert foreground and background color
  | Blink      -- ^ Blinking
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Named colors, 256 and RGB colors for more capable terminals.
data Color
  = DefaultColor                  -- ^ Default terminal color (terminal specific)
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | DullBlack
  | DullRed
  | DullGreen
  | DullYellow
  | DullBlue
  | DullMagenta
  | DullCyan
  | DullWhite
  | Color256 !Word8               -- ^ Color from 256 color scheme. Color is automatically reduced to 8 colors for less capable terminals.
  | RGB      !Word8 !Word8 !Word8 -- ^ True color. Color is automatically reduced to 256 or 8 colors for less capable terminals.
  deriving (Eq, Ord, Show, Read, Generic)

reduceColor :: Term -> Color -> Color
reduceColor Term8    = reduceColor8
reduceColor Term256  = reduceColor256
reduceColor _        = id

rgbToWord8 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
rgbToWord8 base q r g b = base * (base * (r `div` q) + (g `div` q)) + (b `div` q)

gray24ToANSI :: Word8 -> Color
gray24ToANSI x
  | x < 6             = DullBlack
  | x >= 6 && x < 12  = Black
  | x >= 12 && x < 18 = DullWhite
  | otherwise         = White

color216ToANSI :: Word8 -> Color
color216ToANSI x = rgbToANSI 3 r g b
  where (r,gb) = divMod x 36
        (g,b)  = divMod gb 6

color16ToANSI :: Word8 -> Color
color16ToANSI 0  = DullBlack
color16ToANSI 1  = DullRed
color16ToANSI 2  = DullGreen
color16ToANSI 3  = DullYellow
color16ToANSI 4  = DullBlue
color16ToANSI 5  = DullMagenta
color16ToANSI 6  = DullCyan
color16ToANSI 7  = DullWhite
color16ToANSI 8  = Black
color16ToANSI 9  = Red
color16ToANSI 10 = Green
color16ToANSI 11 = Yellow
color16ToANSI 12 = Blue
color16ToANSI 13 = Magenta
color16ToANSI 14 = Cyan
color16ToANSI _  = White

squareNorm :: Integral a => a -> a -> a -> a
squareNorm r g b = ri*ri + bi*bi * gi*gi
  where ri = fromIntegral r
        gi = fromIntegral g
        bi = fromIntegral b

rgbToANSI :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgbToANSI q r g b = color16ToANSI $ bool 0 8 (squareNorm r g b >= squareNorm q q q) + rgbToWord8 2 q b g r

reduceColor8 :: Color -> Color
reduceColor8 (Color256 x)
  | x < 16    = color16ToANSI x
  | x < 232   = color216ToANSI $ x - 16
  | otherwise = gray24ToANSI $ x - 232
reduceColor8 (RGB r g b) = rgbToANSI 128 r g b
reduceColor8 x = x

reduceColor256 :: Color -> Color
reduceColor256 (RGB r g b)
  | r == g && r == b = Color256 $ 232 + r `div` 11
  | otherwise = Color256 $ 16 + rgbToWord8 6 43 r g b
reduceColor256 x = x
