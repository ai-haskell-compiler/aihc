module Termbox.Bindings.Hs.Internal.Cell
  ( Tb_cell (..),
    cellToCCell,
  )
where

import GHC.Generics (Generic)
import qualified Termbox.Bindings.C as Termbox
import Termbox.Bindings.Hs.Internal.Attrs (Tb_attrs (..))
import Termbox.Bindings.Hs.Internal.Prelude (charToWord32)

-- | A cell.
data Tb_cell = Tb_cell
  { -- | A unicode character.
    ch :: {-# UNPACK #-} !Char,
    -- | Foreground attributes.
    fg :: {-# UNPACK #-} !Tb_attrs,
    -- | Background attributes.
    bg :: {-# UNPACK #-} !Tb_attrs
  }
  deriving stock (Eq, Generic, Show)

cellToCCell :: Tb_cell -> Termbox.Tb_cell
cellToCCell Tb_cell {ch, fg = Tb_attrs fg, bg = Tb_attrs bg} =
  Termbox.Tb_cell
    { ch = charToWord32 ch,
      fg,
      bg
    }
{-# INLINE cellToCCell #-}
