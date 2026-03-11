{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Regions where

import Foreign.ForeignPtr

newtype CairoRegionT s = CairoRegionT (ForeignPtr (CairoRegionT s))
