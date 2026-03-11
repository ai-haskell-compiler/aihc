module Termbox.Bindings.Hs.Internal.Attrs
  ( Tb_attrs (Tb_attrs),
    _TB_DEFAULT,
    _TB_BLACK,
    _TB_BLUE,
    _TB_CYAN,
    _TB_GREEN,
    _TB_MAGENTA,
    _TB_RED,
    _TB_WHITE,
    _TB_YELLOW,
    _TB_BOLD,
    _TB_REVERSE,
    _TB_UNDERLINE,
  )
where

import Data.Bits ((.|.))
import Data.Coerce (coerce)
import Data.Word (Word16)
import qualified Termbox.Bindings.C as Termbox

-- | Cell attributes.
newtype Tb_attrs
  = Tb_attrs Word16
  deriving stock (Eq, Show)

instance Monoid Tb_attrs where
  mempty = Tb_attrs 0

instance Semigroup Tb_attrs where
  (<>) = coerce @(Word16 -> Word16 -> Word16) (.|.)

_TB_DEFAULT :: Tb_attrs
_TB_DEFAULT =
  Tb_attrs Termbox._TB_DEFAULT

_TB_BLACK :: Tb_attrs
_TB_BLACK =
  Tb_attrs Termbox._TB_BLACK

_TB_BLUE :: Tb_attrs
_TB_BLUE =
  Tb_attrs Termbox._TB_BLUE

_TB_CYAN :: Tb_attrs
_TB_CYAN =
  Tb_attrs Termbox._TB_CYAN

_TB_GREEN :: Tb_attrs
_TB_GREEN =
  Tb_attrs Termbox._TB_GREEN

_TB_MAGENTA :: Tb_attrs
_TB_MAGENTA =
  Tb_attrs Termbox._TB_MAGENTA

_TB_RED :: Tb_attrs
_TB_RED =
  Tb_attrs Termbox._TB_RED

_TB_WHITE :: Tb_attrs
_TB_WHITE =
  Tb_attrs Termbox._TB_WHITE

_TB_YELLOW :: Tb_attrs
_TB_YELLOW =
  Tb_attrs Termbox._TB_YELLOW

_TB_BOLD :: Tb_attrs
_TB_BOLD =
  Tb_attrs Termbox._TB_BOLD

_TB_REVERSE :: Tb_attrs
_TB_REVERSE =
  Tb_attrs Termbox._TB_REVERSE

_TB_UNDERLINE :: Tb_attrs
_TB_UNDERLINE =
  Tb_attrs Termbox._TB_UNDERLINE
