module Termbox.Bindings.Hs.Internal.InitError
  ( Tb_init_error
      ( Tb_init_error,
        TB_EFAILED_TO_OPEN_TTY,
        TB_EPIPE_TRAP_ERROR,
        TB_EUNSUPPORTED_TERMINAL
      ),
  )
where

import Control.Exception (Exception)
import Foreign.C.Types (CInt)
import Termbox.Bindings.C (_TB_EFAILED_TO_OPEN_TTY, _TB_EPIPE_TRAP_ERROR, _TB_EUNSUPPORTED_TERMINAL)

-- | A 'tb_init' error.
newtype Tb_init_error
  = Tb_init_error CInt
  deriving anyclass (Exception)
  deriving stock (Eq)

instance Show Tb_init_error where
  show = \case
    TB_EFAILED_TO_OPEN_TTY -> "TB_EFAILED_TO_OPEN_TTY"
    TB_EPIPE_TRAP_ERROR -> "TB_EPIPE_TRAP_ERROR"
    TB_EUNSUPPORTED_TERMINAL -> "TB_EUNSUPPORTED_TERMINAL"

pattern TB_EFAILED_TO_OPEN_TTY :: Tb_init_error
pattern TB_EFAILED_TO_OPEN_TTY <-
  ((== Tb_init_error _TB_EFAILED_TO_OPEN_TTY) -> True)
  where
    TB_EFAILED_TO_OPEN_TTY = Tb_init_error _TB_EFAILED_TO_OPEN_TTY

pattern TB_EPIPE_TRAP_ERROR :: Tb_init_error
pattern TB_EPIPE_TRAP_ERROR <-
  ((== Tb_init_error Termbox.Bindings.C._TB_EPIPE_TRAP_ERROR) -> True)
  where
    TB_EPIPE_TRAP_ERROR = Tb_init_error _TB_EPIPE_TRAP_ERROR

pattern TB_EUNSUPPORTED_TERMINAL :: Tb_init_error
pattern TB_EUNSUPPORTED_TERMINAL <-
  ((== Tb_init_error _TB_EUNSUPPORTED_TERMINAL) -> True)
  where
    TB_EUNSUPPORTED_TERMINAL = Tb_init_error _TB_EUNSUPPORTED_TERMINAL

-- N.B. This requires Tb_init_error to remain abstract
{-# COMPLETE TB_EFAILED_TO_OPEN_TTY, TB_EPIPE_TRAP_ERROR, TB_EUNSUPPORTED_TERMINAL #-}
