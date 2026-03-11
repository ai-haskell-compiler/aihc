module Termbox.Bindings.Hs.Internal.OutputMode
  ( Tb_output_mode
      ( Tb_output_mode,
        TB_OUTPUT_216,
        TB_OUTPUT_256,
        TB_OUTPUT_GRAYSCALE,
        TB_OUTPUT_NORMAL
      ),
  )
where

import Foreign.C.Types (CInt)
import Termbox.Bindings.C
  ( _TB_OUTPUT_216,
    _TB_OUTPUT_256,
    _TB_OUTPUT_GRAYSCALE,
    _TB_OUTPUT_NORMAL,
  )

-- | The output mode.
newtype Tb_output_mode
  = Tb_output_mode CInt
  deriving stock (Eq)

instance Show Tb_output_mode where
  show = \case
    TB_OUTPUT_216 -> "TB_OUTPUT_216"
    TB_OUTPUT_256 -> "TB_OUTPUT_256"
    TB_OUTPUT_GRAYSCALE -> "TB_OUTPUT_GRAYSCALE"
    TB_OUTPUT_NORMAL -> "TB_OUTPUT_NORMAL"

pattern TB_OUTPUT_216 :: Tb_output_mode
pattern TB_OUTPUT_216 <-
  ((== Tb_output_mode _TB_OUTPUT_216) -> True)
  where
    TB_OUTPUT_216 = Tb_output_mode _TB_OUTPUT_216

pattern TB_OUTPUT_256 :: Tb_output_mode
pattern TB_OUTPUT_256 <-
  ((== Tb_output_mode _TB_OUTPUT_256) -> True)
  where
    TB_OUTPUT_256 = Tb_output_mode _TB_OUTPUT_256

pattern TB_OUTPUT_GRAYSCALE :: Tb_output_mode
pattern TB_OUTPUT_GRAYSCALE <-
  ((== Tb_output_mode _TB_OUTPUT_GRAYSCALE) -> True)
  where
    TB_OUTPUT_GRAYSCALE = Tb_output_mode _TB_OUTPUT_GRAYSCALE

pattern TB_OUTPUT_NORMAL :: Tb_output_mode
pattern TB_OUTPUT_NORMAL <-
  ((== Tb_output_mode _TB_OUTPUT_NORMAL) -> True)
  where
    TB_OUTPUT_NORMAL = Tb_output_mode _TB_OUTPUT_NORMAL

-- N.B. This requires Tb_output_mode to remain abstract
{-# COMPLETE TB_OUTPUT_216, TB_OUTPUT_256, TB_OUTPUT_GRAYSCALE, TB_OUTPUT_NORMAL #-}
