module Text.ANSI
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

import Data.Text (Text)
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
black :: Text -> Text
black =
  lift Builder.ANSI.black
{-# INLINE black #-}

-- | Red foreground.
red :: Text -> Text
red =
  lift Builder.ANSI.red
{-# INLINE red #-}

-- | Green foreground.
green :: Text -> Text
green =
  lift Builder.ANSI.green
{-# INLINE green #-}

-- | Yellow foreground.
yellow :: Text -> Text
yellow =
  lift Builder.ANSI.yellow
{-# INLINE yellow #-}

-- | Blue foreground.
blue :: Text -> Text
blue =
  lift Builder.ANSI.blue
{-# INLINE blue #-}

-- | Magenta foreground.
magenta :: Text -> Text
magenta =
  lift Builder.ANSI.magenta
{-# INLINE magenta #-}

-- | Cyan foreground.
cyan :: Text -> Text
cyan =
  lift Builder.ANSI.cyan
{-# INLINE cyan #-}

-- | White foreground.
white :: Text -> Text
white =
  lift Builder.ANSI.white
{-# INLINE white #-}

-- | Bright black foreground.
brightBlack :: Text -> Text
brightBlack =
  lift Builder.ANSI.brightBlack
{-# INLINE brightBlack #-}

-- | Bright red foreground.
brightRed :: Text -> Text
brightRed =
  lift Builder.ANSI.brightRed
{-# INLINE brightRed #-}

-- | Bright green foreground.
brightGreen :: Text -> Text
brightGreen =
  lift Builder.ANSI.brightGreen
{-# INLINE brightGreen #-}

-- | Bright yellow foreground.
brightYellow :: Text -> Text
brightYellow =
  lift Builder.ANSI.brightYellow
{-# INLINE brightYellow #-}

-- | Bright blue foreground.
brightBlue :: Text -> Text
brightBlue =
  lift Builder.ANSI.brightBlue
{-# INLINE brightBlue #-}

-- | Bright magenta foreground.
brightMagenta :: Text -> Text
brightMagenta =
  lift Builder.ANSI.brightMagenta
{-# INLINE brightMagenta #-}

-- | Bright cyan foreground.
brightCyan :: Text -> Text
brightCyan =
  lift Builder.ANSI.brightCyan
{-# INLINE brightCyan #-}

-- | Bright white foreground.
brightWhite :: Text -> Text
brightWhite =
  lift Builder.ANSI.brightWhite
{-# INLINE brightWhite #-}

-- | RGB foreground.
rgb :: Word8 -> Word8 -> Word8 -> Text -> Text
rgb r g b =
  lift (Builder.ANSI.rgb r g b)
{-# INLINE rgb #-}

-- | Black background.
blackBg :: Text -> Text
blackBg =
  lift Builder.ANSI.blackBg
{-# INLINE blackBg #-}

-- | Red background.
redBg :: Text -> Text
redBg =
  lift Builder.ANSI.redBg
{-# INLINE redBg #-}

-- | Green background.
greenBg :: Text -> Text
greenBg =
  lift Builder.ANSI.greenBg
{-# INLINE greenBg #-}

-- | Yellow background.
yellowBg :: Text -> Text
yellowBg =
  lift Builder.ANSI.yellowBg
{-# INLINE yellowBg #-}

-- | Blue background.
blueBg :: Text -> Text
blueBg =
  lift Builder.ANSI.blueBg
{-# INLINE blueBg #-}

-- | Magenta background.
magentaBg :: Text -> Text
magentaBg =
  lift Builder.ANSI.magentaBg
{-# INLINE magentaBg #-}

-- | Cyan background.
cyanBg :: Text -> Text
cyanBg =
  lift Builder.ANSI.cyanBg
{-# INLINE cyanBg #-}

-- | White background.
whiteBg :: Text -> Text
whiteBg =
  lift Builder.ANSI.whiteBg
{-# INLINE whiteBg #-}

-- | Bright black background.
brightBlackBg :: Text -> Text
brightBlackBg =
  lift Builder.ANSI.brightBlackBg
{-# INLINE brightBlackBg #-}

-- | Bright red background.
brightRedBg :: Text -> Text
brightRedBg =
  lift Builder.ANSI.brightRedBg
{-# INLINE brightRedBg #-}

-- | Bright green background.
brightGreenBg :: Text -> Text
brightGreenBg =
  lift Builder.ANSI.brightGreenBg
{-# INLINE brightGreenBg #-}

-- | Bright yellow background.
brightYellowBg :: Text -> Text
brightYellowBg =
  lift Builder.ANSI.brightYellowBg
{-# INLINE brightYellowBg #-}

-- | Bright blue background.
brightBlueBg :: Text -> Text
brightBlueBg =
  lift Builder.ANSI.brightBlueBg
{-# INLINE brightBlueBg #-}

-- | Bright magenta background.
brightMagentaBg :: Text -> Text
brightMagentaBg =
  lift Builder.ANSI.brightMagentaBg
{-# INLINE brightMagentaBg #-}

-- | Bright cyan background.
brightCyanBg :: Text -> Text
brightCyanBg =
  lift Builder.ANSI.brightCyanBg
{-# INLINE brightCyanBg #-}

-- | Bright white background.
brightWhiteBg :: Text -> Text
brightWhiteBg =
  lift Builder.ANSI.brightWhiteBg
{-# INLINE brightWhiteBg #-}

-- | RGB background.
rgbBg :: Word8 -> Word8 -> Word8 -> Text -> Text
rgbBg r g b =
  lift (Builder.ANSI.rgbBg r g b)
{-# INLINE rgbBg #-}

-- | __Bold__ style (high intensity).
bold :: Text -> Text
bold =
  lift Builder.ANSI.bold
{-# INLINE bold #-}

-- | Faint style (low intensity).
faint :: Text -> Text
faint =
  lift Builder.ANSI.faint
{-# INLINE faint #-}

-- | /Italic/ style.
italic :: Text -> Text
italic =
  lift Builder.ANSI.italic
{-# INLINE italic #-}

-- | U̲n̲d̲e̲r̲l̲i̲n̲e̲ style.
underline :: Text -> Text
underline =
  lift Builder.ANSI.underline
{-# INLINE underline #-}

-- | D̳o̳u̳b̳l̳e̳ ̳u̳n̳d̳e̳r̳l̳i̳n̳e̳ style.
doubleUnderline :: Text -> Text
doubleUnderline =
  lift Builder.ANSI.doubleUnderline
{-# INLINE doubleUnderline #-}

-- | S̶t̶r̶i̶k̶e̶t̶h̶r̶o̶u̶g̶h̶ style.
strikethrough :: Text -> Text
strikethrough =
  lift Builder.ANSI.strikethrough
{-# INLINE strikethrough #-}

-- | Frame style.
frame :: Text -> Text
frame =
  lift Builder.ANSI.frame
{-# INLINE frame #-}

-- | Encircle style.
encircle :: Text -> Text
encircle =
  lift Builder.ANSI.encircle
{-# INLINE encircle #-}

-- | O̅v̅e̅r̅l̅i̅n̅e̅ style.
overline :: Text -> Text
overline =
  lift Builder.ANSI.overline
{-# INLINE overline #-}

--

lift :: (Builder -> Builder) -> Text -> Text
lift f =
  Builder.runBuilder . f . Builder.fromText
-- Don't inline before phase 2
{-# NOINLINE [2] lift #-}

-- Collapse lift/lift to a single lift before phase 2
{-# RULES
"lift/lift" [~2] forall f g s.
  lift f (lift g s) =
    lift (f . g) s
  #-}
