module Text.Lazy.Builder.ANSI
  ( -- $intro

    -- * Foreground color
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    brightBlack,
    brightRed,
    brightGreen,
    brightYellow,
    brightBlue,
    brightMagenta,
    brightCyan,
    brightWhite,
    rgb,

    -- * Background color
    blackBg,
    redBg,
    greenBg,
    yellowBg,
    blueBg,
    magentaBg,
    cyanBg,
    whiteBg,
    brightBlackBg,
    brightRedBg,
    brightGreenBg,
    brightYellowBg,
    brightBlueBg,
    brightMagentaBg,
    brightCyanBg,
    brightWhiteBg,
    rgbBg,

    -- * Style
    bold,
    faint,
    italic,
    underline,
    doubleUnderline,
    strikethrough,
    frame,
    encircle,
    overline,
  )
where

import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Internals (c_isatty)

-- $intro
--
-- Text styling for ANSI terminals using SGR codes, as defined by the
-- <https://www.ecma-international.org/publications-and-standards/standards/ecma-48 ECMA-48>
-- standard.
--
-- Supports foreground\/background color, bold\/faint intensity, italic,
-- single\/double underline, strikethrough, frame, encircle, and overline escape
-- sequences. Some styles may not work on your terminal.
--
-- Also features terminal detection, so redirecting styled output to a file will
-- automatically strip the ANSI escape sequences.

-- | Black foreground.
black :: Builder -> Builder
black =
  foreground (Builder.fromString "30")
{-# INLINE black #-}

-- | Red foreground.
red :: Builder -> Builder
red =
  foreground (Builder.fromString "31")
{-# INLINE red #-}

-- | Green foreground.
green :: Builder -> Builder
green =
  foreground (Builder.fromString "32")
{-# INLINE green #-}

-- | Yellow foreground.
yellow :: Builder -> Builder
yellow =
  foreground (Builder.fromString "33")
{-# INLINE yellow #-}

-- | Blue foreground.
blue :: Builder -> Builder
blue =
  foreground (Builder.fromString "34")
{-# INLINE blue #-}

-- | Magenta foreground.
magenta :: Builder -> Builder
magenta =
  foreground (Builder.fromString "35")
{-# INLINE magenta #-}

-- | Cyan foreground.
cyan :: Builder -> Builder
cyan =
  foreground (Builder.fromString "36")
{-# INLINE cyan #-}

-- | White foreground.
white :: Builder -> Builder
white =
  foreground (Builder.fromString "37")
{-# INLINE white #-}

-- | Bright black foreground.
brightBlack :: Builder -> Builder
brightBlack =
  foreground (Builder.fromString "90")
{-# INLINE brightBlack #-}

-- | Bright red foreground.
brightRed :: Builder -> Builder
brightRed =
  foreground (Builder.fromString "91")
{-# INLINE brightRed #-}

-- | Bright green foreground.
brightGreen :: Builder -> Builder
brightGreen =
  foreground (Builder.fromString "92")
{-# INLINE brightGreen #-}

-- | Bright yellow foreground.
brightYellow :: Builder -> Builder
brightYellow =
  foreground (Builder.fromString "93")
{-# INLINE brightYellow #-}

-- | Bright blue foreground.
brightBlue :: Builder -> Builder
brightBlue =
  foreground (Builder.fromString "94")
{-# INLINE brightBlue #-}

-- | Bright magenta foreground.
brightMagenta :: Builder -> Builder
brightMagenta =
  foreground (Builder.fromString "95")
{-# INLINE brightMagenta #-}

-- | Bright cyan foreground.
brightCyan :: Builder -> Builder
brightCyan =
  foreground (Builder.fromString "96")
{-# INLINE brightCyan #-}

-- | Bright white foreground.
brightWhite :: Builder -> Builder
brightWhite =
  foreground (Builder.fromString "97")
{-# INLINE brightWhite #-}

-- | RGB foreground.
rgb :: Word8 -> Word8 -> Word8 -> Builder -> Builder
rgb r g b =
  foreground (Builder.fromString "38;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINE rgb #-}

foreground :: Builder -> Builder -> Builder
foreground s =
  surround s (Builder.fromString "39")
{-# INLINE foreground #-}

-- | Black background.
blackBg :: Builder -> Builder
blackBg =
  background (Builder.fromString "40")
{-# INLINE blackBg #-}

-- | Red background.
redBg :: Builder -> Builder
redBg =
  background (Builder.fromString "41")
{-# INLINE redBg #-}

-- | Green background.
greenBg :: Builder -> Builder
greenBg =
  background (Builder.fromString "42")
{-# INLINE greenBg #-}

-- | Yellow background.
yellowBg :: Builder -> Builder
yellowBg =
  background (Builder.fromString "43")
{-# INLINE yellowBg #-}

-- | Blue background.
blueBg :: Builder -> Builder
blueBg =
  background (Builder.fromString "44")
{-# INLINE blueBg #-}

-- | Magenta background.
magentaBg :: Builder -> Builder
magentaBg =
  background (Builder.fromString "45")
{-# INLINE magentaBg #-}

-- | Cyan background.
cyanBg :: Builder -> Builder
cyanBg =
  background (Builder.fromString "46")
{-# INLINE cyanBg #-}

-- | White background.
whiteBg :: Builder -> Builder
whiteBg =
  background (Builder.fromString "47")
{-# INLINE whiteBg #-}

-- | Bright black background.
brightBlackBg :: Builder -> Builder
brightBlackBg =
  background (Builder.fromString "100")
{-# INLINE brightBlackBg #-}

-- | Bright red background.
brightRedBg :: Builder -> Builder
brightRedBg =
  background (Builder.fromString "101")
{-# INLINE brightRedBg #-}

-- | Bright green background.
brightGreenBg :: Builder -> Builder
brightGreenBg =
  background (Builder.fromString "102")
{-# INLINE brightGreenBg #-}

-- | Bright yellow background.
brightYellowBg :: Builder -> Builder
brightYellowBg =
  background (Builder.fromString "103")
{-# INLINE brightYellowBg #-}

-- | Bright blue background.
brightBlueBg :: Builder -> Builder
brightBlueBg =
  background (Builder.fromString "104")
{-# INLINE brightBlueBg #-}

-- | Bright magenta background.
brightMagentaBg :: Builder -> Builder
brightMagentaBg =
  background (Builder.fromString "105")
{-# INLINE brightMagentaBg #-}

-- | Bright cyan background.
brightCyanBg :: Builder -> Builder
brightCyanBg =
  background (Builder.fromString "106")
{-# INLINE brightCyanBg #-}

-- | Bright white background.
brightWhiteBg :: Builder -> Builder
brightWhiteBg =
  background (Builder.fromString "107")
{-# INLINE brightWhiteBg #-}

background :: Builder -> Builder -> Builder
background s =
  surround s (Builder.fromString "49")
{-# INLINE background #-}

-- | RGB background.
rgbBg :: Word8 -> Word8 -> Word8 -> Builder -> Builder
rgbBg r g b =
  background (Builder.fromString "48;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINE rgbBg #-}

-- | __Bold__ style (high intensity).
bold :: Builder -> Builder
bold =
  surround (Builder.fromString "1") (Builder.fromString "22")
{-# INLINE bold #-}

-- | Faint style (low intensity).
faint :: Builder -> Builder
faint =
  surround (Builder.fromString "2") (Builder.fromString "22")
{-# INLINE faint #-}

-- | /Italic/ style.
italic :: Builder -> Builder
italic =
  surround (Builder.fromString "3") (Builder.fromString "23")
{-# INLINE italic #-}

-- | U̲n̲d̲e̲r̲l̲i̲n̲e̲ style.
underline :: Builder -> Builder
underline =
  surround (Builder.fromString "4") (Builder.fromString "24")
{-# INLINE underline #-}

-- | D̳o̳u̳b̳l̳e̳ ̳u̳n̳d̳e̳r̳l̳i̳n̳e̳ style.
doubleUnderline :: Builder -> Builder
doubleUnderline =
  surround (Builder.fromString "21") (Builder.fromString "24")
{-# INLINE doubleUnderline #-}

-- | S̶t̶r̶i̶k̶e̶t̶h̶r̶o̶u̶g̶h̶ style.
strikethrough :: Builder -> Builder
strikethrough =
  surround (Builder.fromString "9") (Builder.fromString "29")
{-# INLINE strikethrough #-}

-- | Frame style.
frame :: Builder -> Builder
frame =
  surround (Builder.fromString "51") (Builder.fromString "54")
{-# INLINE frame #-}

-- | Encircle style.
encircle :: Builder -> Builder
encircle =
  surround (Builder.fromString "52") (Builder.fromString "54")
{-# INLINE encircle #-}

-- | O̅v̅e̅r̅l̅i̅n̅e̅ style.
overline :: Builder -> Builder
overline =
  surround (Builder.fromString "53") (Builder.fromString "55")
{-# INLINE overline #-}

--------------------------------------------------------------------------------

surround :: Builder -> Builder -> Builder -> Builder
surround open close text
  | isatty = esc <> open <> m <> text <> esc <> close <> m
  | otherwise = text
-- Don't inline before phase 1
{-# NOINLINE [1] surround #-}

esc :: Builder
esc =
  Builder.fromString "\ESC["

m :: Builder
m =
  Builder.singleton 'm'

semi :: Builder
semi =
  Builder.singleton ';'

isatty :: Bool
isatty =
  unsafePerformIO (c_isatty 1) == 1
{-# NOINLINE isatty #-}

-- Collapse surround/surround to a single surround before phase 1
{-# RULES
"surround/surround" [~1] forall a b c d s.
  surround a b (surround c d s) =
    surround (a <> semi <> c) (b <> semi <> d) s
  #-}
