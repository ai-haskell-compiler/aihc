module String.ANSI
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

import qualified Data.Text as Text
import Data.Text.Builder.Linear (Builder)
import qualified Data.Text.Builder.Linear as Builder
import Data.Word (Word8)
import qualified Text.Builder.ANSI as Builder.ANSI

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
black :: String -> String
black =
  lift Builder.ANSI.black
{-# INLINE black #-}

-- | Red foreground.
red :: String -> String
red =
  lift Builder.ANSI.red
{-# INLINE red #-}

-- | Green foreground.
green :: String -> String
green =
  lift Builder.ANSI.green
{-# INLINE green #-}

-- | Yellow foreground.
yellow :: String -> String
yellow =
  lift Builder.ANSI.yellow
{-# INLINE yellow #-}

-- | Blue foreground.
blue :: String -> String
blue =
  lift Builder.ANSI.blue
{-# INLINE blue #-}

-- | Magenta foreground.
magenta :: String -> String
magenta =
  lift Builder.ANSI.magenta
{-# INLINE magenta #-}

-- | Cyan foreground.
cyan :: String -> String
cyan =
  lift Builder.ANSI.cyan
{-# INLINE cyan #-}

-- | White foreground.
white :: String -> String
white =
  lift Builder.ANSI.white
{-# INLINE white #-}

-- | Bright black foreground.
brightBlack :: String -> String
brightBlack =
  lift Builder.ANSI.brightBlack
{-# INLINE brightBlack #-}

-- | Bright red foreground.
brightRed :: String -> String
brightRed =
  lift Builder.ANSI.brightRed
{-# INLINE brightRed #-}

-- | Bright green foreground.
brightGreen :: String -> String
brightGreen =
  lift Builder.ANSI.brightGreen
{-# INLINE brightGreen #-}

-- | Bright yellow foreground.
brightYellow :: String -> String
brightYellow =
  lift Builder.ANSI.brightYellow
{-# INLINE brightYellow #-}

-- | Bright blue foreground.
brightBlue :: String -> String
brightBlue =
  lift Builder.ANSI.brightBlue
{-# INLINE brightBlue #-}

-- | Bright magenta foreground.
brightMagenta :: String -> String
brightMagenta =
  lift Builder.ANSI.brightMagenta
{-# INLINE brightMagenta #-}

-- | Bright cyan foreground.
brightCyan :: String -> String
brightCyan =
  lift Builder.ANSI.brightCyan
{-# INLINE brightCyan #-}

-- | Bright white foreground.
brightWhite :: String -> String
brightWhite =
  lift Builder.ANSI.brightWhite
{-# INLINE brightWhite #-}

-- | RGB foreground.
rgb :: Word8 -> Word8 -> Word8 -> String -> String
rgb r g b =
  lift (Builder.ANSI.rgb r g b)
{-# INLINE rgb #-}

-- | Black background.
blackBg :: String -> String
blackBg =
  lift Builder.ANSI.blackBg
{-# INLINE blackBg #-}

-- | Red background.
redBg :: String -> String
redBg =
  lift Builder.ANSI.redBg
{-# INLINE redBg #-}

-- | Green background.
greenBg :: String -> String
greenBg =
  lift Builder.ANSI.greenBg
{-# INLINE greenBg #-}

-- | Yellow background.
yellowBg :: String -> String
yellowBg =
  lift Builder.ANSI.yellowBg
{-# INLINE yellowBg #-}

-- | Blue background.
blueBg :: String -> String
blueBg =
  lift Builder.ANSI.blueBg
{-# INLINE blueBg #-}

-- | Magenta background.
magentaBg :: String -> String
magentaBg =
  lift Builder.ANSI.magentaBg
{-# INLINE magentaBg #-}

-- | Cyan background.
cyanBg :: String -> String
cyanBg =
  lift Builder.ANSI.cyanBg
{-# INLINE cyanBg #-}

-- | White background.
whiteBg :: String -> String
whiteBg =
  lift Builder.ANSI.whiteBg
{-# INLINE whiteBg #-}

-- | Bright black background.
brightBlackBg :: String -> String
brightBlackBg =
  lift Builder.ANSI.brightBlackBg
{-# INLINE brightBlackBg #-}

-- | Bright red background.
brightRedBg :: String -> String
brightRedBg =
  lift Builder.ANSI.brightRedBg
{-# INLINE brightRedBg #-}

-- | Bright green background.
brightGreenBg :: String -> String
brightGreenBg =
  lift Builder.ANSI.brightGreenBg
{-# INLINE brightGreenBg #-}

-- | Bright yellow background.
brightYellowBg :: String -> String
brightYellowBg =
  lift Builder.ANSI.brightYellowBg
{-# INLINE brightYellowBg #-}

-- | Bright blue background.
brightBlueBg :: String -> String
brightBlueBg =
  lift Builder.ANSI.brightBlueBg
{-# INLINE brightBlueBg #-}

-- | Bright magenta background.
brightMagentaBg :: String -> String
brightMagentaBg =
  lift Builder.ANSI.brightMagentaBg
{-# INLINE brightMagentaBg #-}

-- | Bright cyan background.
brightCyanBg :: String -> String
brightCyanBg =
  lift Builder.ANSI.brightCyanBg
{-# INLINE brightCyanBg #-}

-- | Bright white background.
brightWhiteBg :: String -> String
brightWhiteBg =
  lift Builder.ANSI.brightWhiteBg
{-# INLINE brightWhiteBg #-}

-- | RGB background.
rgbBg :: Word8 -> Word8 -> Word8 -> String -> String
rgbBg r g b =
  lift (Builder.ANSI.rgbBg r g b)
{-# INLINE rgbBg #-}

-- | __Bold__ style (high intensity).
bold :: String -> String
bold =
  lift Builder.ANSI.bold
{-# INLINE bold #-}

-- | Faint style (low intensity).
faint :: String -> String
faint =
  lift Builder.ANSI.faint
{-# INLINE faint #-}

-- | /Italic/ style.
italic :: String -> String
italic =
  lift Builder.ANSI.italic
{-# INLINE italic #-}

-- | U̲n̲d̲e̲r̲l̲i̲n̲e̲ style.
underline :: String -> String
underline =
  lift Builder.ANSI.underline
{-# INLINE underline #-}

-- | D̳o̳u̳b̳l̳e̳ ̳u̳n̳d̳e̳r̳l̳i̳n̳e̳ style.
doubleUnderline :: String -> String
doubleUnderline =
  lift Builder.ANSI.doubleUnderline
{-# INLINE doubleUnderline #-}

-- | S̶t̶r̶i̶k̶e̶t̶h̶r̶o̶u̶g̶h̶ style.
strikethrough :: String -> String
strikethrough =
  lift Builder.ANSI.strikethrough
{-# INLINE strikethrough #-}

-- | Frame style.
frame :: String -> String
frame =
  lift Builder.ANSI.frame
{-# INLINE frame #-}

-- | Encircle style.
encircle :: String -> String
encircle =
  lift Builder.ANSI.encircle
{-# INLINE encircle #-}

-- | O̅v̅e̅r̅l̅i̅n̅e̅ style.
overline :: String -> String
overline =
  lift Builder.ANSI.overline
{-# INLINE overline #-}

--

lift :: (Builder -> Builder) -> String -> String
lift f =
  Text.unpack . Builder.runBuilder . f . foldMap Builder.fromChar
-- Don't inline before phase 2
{-# NOINLINE [2] lift #-}

-- Collapse lift/lift to a single lift before phase 2
{-# RULES
"lift/lift" [~2] forall f g s.
  lift f (lift g s) =
    lift (f . g) s
  #-}
