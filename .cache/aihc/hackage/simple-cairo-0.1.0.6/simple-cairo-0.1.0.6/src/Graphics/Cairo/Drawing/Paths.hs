{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths (
	cairoClosePath, cairoArc, cairoLineTo, cairoMoveTo, cairoRectangle,
	cairoRelCurveTo, cairoRelLineTo,

	cairoNewPath
	) where

import Graphics.Cairo.Drawing.Paths.Basic
import Graphics.Cairo.Drawing.Paths.Relative
