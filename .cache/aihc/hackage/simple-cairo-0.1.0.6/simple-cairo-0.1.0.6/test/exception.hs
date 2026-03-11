{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Exception

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces

import Graphics.Cairo.Exception

import Data.CairoImage.Internal

main :: IO ()
main = mainGen `catch` (print @CairoStatus)

mainGen :: IO ()
mainGen = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 500 500
	cr <- cairoCreate s
	cairoLineTo cr 100 100
	cairoNewPath cr
	cairoLineTo cr 0 0
	cairoStroke cr
	cairoPopGroupToSource cr
	print =<< cairoImageSurfaceGetCairoImage s
