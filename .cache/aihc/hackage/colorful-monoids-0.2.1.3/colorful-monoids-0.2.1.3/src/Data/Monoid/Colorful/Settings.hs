module Data.Monoid.Colorful.Settings (
  Settings(..)
  , defaultSettings
  , setStyle, setBg, setFg
  , resetStack, pushStack, popStack
) where

import Data.Monoid.Colorful.Color

data Settings = Settings
  { settingBold   :: !Bool
  , settingItalic :: !Bool
  , settingUnderline  :: !Bool
  , settingInvert :: !Bool
  , settingBlink  :: !Bool
  , settingFg     :: !Color
  , settingBg     :: !Color
  } deriving (Eq)

type SettingsStack = (Settings, [Settings])

defaultSettings :: Settings
defaultSettings = Settings
  { settingBold      = False
  , settingItalic    = False
  , settingInvert    = False
  , settingUnderline = False
  , settingBlink     = False
  , settingFg        = DefaultColor
  , settingBg        = DefaultColor
  }

setStyle :: Style -> Bool -> Settings -> Settings
setStyle Bold      b s = s { settingBold      = b }
setStyle Italic    b s = s { settingItalic    = b }
setStyle Underline b s = s { settingUnderline = b }
setStyle Invert    b s = s { settingInvert    = b }
setStyle Blink     b s = s { settingBlink     = b }
{-# INLINE setStyle #-}

setBg, setFg :: Color -> Settings -> Settings
setBg c s = s { settingBg = c }
setFg c s = s { settingFg = c }
{-# INLINE setBg #-}
{-# INLINE setFg #-}

resetStack, pushStack, popStack :: SettingsStack -> SettingsStack
resetStack (_, ys)     = (defaultSettings, ys)
pushStack  (y, ys)     = (y, y:ys)
popStack   (_, [])     = (defaultSettings, [])
popStack   (_, z : zs) = (z, zs)
{-# INLINE resetStack #-}
{-# INLINE pushStack #-}
{-# INLINE popStack #-}
