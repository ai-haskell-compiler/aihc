module Termbox.Bindings.Hs.Internal.InputMode
  ( Tb_input_mode (Tb_input_mode),
    _TB_INPUT_ALT,
    _TB_INPUT_ESC,
    _TB_INPUT_MOUSE,
  )
where

import Data.Bits (Bits, (.|.))
import Data.Coerce (coerce)
import Foreign.C.Types (CInt)
import qualified Termbox.Bindings.C as Termbox

-- | The input mode.
newtype Tb_input_mode
  = Tb_input_mode CInt
  deriving stock (Eq)
  deriving newtype (Bits)

instance Show Tb_input_mode where
  show mode
    | mode == _TB_INPUT_ESC = "_TB_INPUT_ESC"
    | mode == _TB_INPUT_ALT = "_TB_INPUT_ALT"
    | mode == _TB_INPUT_ESC <> _TB_INPUT_MOUSE = "_TB_INPUT_ESC <> _TB_INPUT_MOUSE"
    | mode == _TB_INPUT_ALT <> _TB_INPUT_MOUSE = "_TB_INPUT_ALT <> _TB_INPUT_MOUSE"
    | otherwise = "Tb_input_mode " ++ show (coerce @_ @CInt mode)

instance Semigroup Tb_input_mode where
  (<>) = coerce ((.|.) :: CInt -> CInt -> CInt)

_TB_INPUT_ALT :: Tb_input_mode
_TB_INPUT_ALT =
  Tb_input_mode Termbox._TB_INPUT_ALT

_TB_INPUT_ESC :: Tb_input_mode
_TB_INPUT_ESC =
  Tb_input_mode Termbox._TB_INPUT_ESC

_TB_INPUT_MOUSE :: Tb_input_mode
_TB_INPUT_MOUSE =
  Tb_input_mode Termbox._TB_INPUT_MOUSE
