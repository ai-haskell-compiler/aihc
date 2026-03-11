module Termbox.Internal.Size
  ( Size (..),
    getSize,
  )
where

import GHC.Generics (Generic)
import qualified Termbox.Bindings.Hs

-- | A terminal size.
data Size = Size
  { width :: {-# UNPACK #-} !Int,
    height :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Generic, Ord, Show)

-- | Get the current terminal size.
getSize :: IO Size
getSize = do
  width <- Termbox.Bindings.Hs.tb_width
  height <- Termbox.Bindings.Hs.tb_height
  pure Size {width, height}
