{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceT (
	IsCairoSurfaceT, CairoSurfaceT,
	cairoSurfaceGetType, cairoSurfaceGetContent,
	cairoSurfaceFlush ) where

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
