{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Extents where

import Foreign.C.Types

data CairoExtents = CairoExtentsLeftTopRightBottom {
	cairoExtentsLeftX :: CDouble,
	cairoExtentsTopY :: CDouble,
	cairoExtentsRightX :: CDouble,
	cairoExtentsBottomY :: CDouble } deriving Show

pattern CairoExtentsLeftTopWidthHeight :: CDouble -> CDouble -> CDouble -> CDouble -> CairoExtents
pattern CairoExtentsLeftTopWidthHeight {
	cairoExtentsLeft, cairoExtentsTop,
	cairoExtentsWidth, cairoExtentsHeight } <-
	(cairoExtentsLeftTopWidthHeight -> (cairoExtentsLeft, cairoExtentsTop, cairoExtentsWidth, cairoExtentsHeight)) where 
	CairoExtentsLeftTopWidthHeight l t w h = CairoExtentsLeftTopRightBottom l t (l + w) (t + h)

cairoExtentsLeftTopWidthHeight :: CairoExtents -> (CDouble, CDouble, CDouble, CDouble)
cairoExtentsLeftTopWidthHeight (CairoExtentsLeftTopRightBottom l t r b) = (l, t, r - l, b - t)
