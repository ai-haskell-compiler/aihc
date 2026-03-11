module Termbox.Internal.Style
  ( Style,
    asForeground,
    asBackground,
    maybeFill,
    fg,
    bg,
    bold,
    underline,
    blink,
  )
where

import Termbox.Bindings.Hs hiding (bg, fg)
import Termbox.Internal.Color (Color, MaybeColor, justColor, nothingColor, unMaybeColor)

data Style = Style
  { foreground :: {-# UNPACK #-} !ColorAndAttrs,
    background :: {-# UNPACK #-} !ColorAndAttrs
  }

instance Monoid Style where
  mempty =
    Style mempty mempty

-- right-biased
instance Semigroup Style where
  Style a1 b1 <> Style a2 b2 =
    Style (a2 <> a1) (b2 <> b1)

-- Render a style as a foreground `Tb_attrs`.
asForeground :: Style -> Tb_attrs
asForeground Style {foreground} =
  renderColorAndAttr foreground

-- Render a style as a background `Tb_attrs`.
asBackground :: Style -> Tb_attrs
asBackground Style {background} =
  renderColorAndAttr background

onlyForeground :: ColorAndAttrs -> Style
onlyForeground style =
  mempty {foreground = style}

onlyBackground :: ColorAndAttrs -> Style
onlyBackground style =
  mempty {background = style}

maybeFill :: MaybeColor -> Style
maybeFill color =
  onlyBackground ColorAndAttrs {color, attrs = mempty}

fg :: Color -> Style
fg =
  onlyForeground . onlyColor

bg :: Color -> Style
bg =
  onlyBackground . onlyColor

bold :: Style
bold =
  onlyForeground (onlyAttr _TB_BOLD)

underline :: Style
underline =
  onlyForeground (onlyAttr _TB_UNDERLINE)

blink :: Style
blink =
  onlyBackground (onlyAttr _TB_BOLD)

data ColorAndAttrs = ColorAndAttrs
  { color :: {-# UNPACK #-} !MaybeColor,
    attrs :: {-# UNPACK #-} !Tb_attrs
  }

instance Monoid ColorAndAttrs where
  mempty =
    ColorAndAttrs nothingColor mempty

-- right-biased
instance Semigroup ColorAndAttrs where
  ColorAndAttrs color1 attr1 <> ColorAndAttrs color2 attr2 =
    ColorAndAttrs
      (if color2 == nothingColor then color1 else color2)
      (attr1 <> attr2)

renderColorAndAttr :: ColorAndAttrs -> Tb_attrs
renderColorAndAttr ColorAndAttrs {color, attrs} =
  attrs <> unMaybeColor color

onlyColor :: Color -> ColorAndAttrs
onlyColor color =
  mempty {color = justColor color}

onlyAttr :: Tb_attrs -> ColorAndAttrs
onlyAttr attrs =
  mempty {attrs}
