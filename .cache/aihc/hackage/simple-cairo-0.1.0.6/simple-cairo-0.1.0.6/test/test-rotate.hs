{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Cairo.Exception
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.PngSupport

import Data.CairoImage.Internal
import Paths_simple_cairo

main :: IO ()
main = do
	putStrLn "*** TEST ROTATE BEGIN ***"
	sfc0 <- CairoSurfaceTImage <$> cairoImageSurfaceCreate CairoFormatArgb32 256 256
	cr <- cairoCreate sfc0
	sfc <- cairoSurfaceCreateFromPng =<< getDataFileName "HaskellLogo.png"
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	cairoStatusToThrowError . (\(CairoStatusT w) -> w) =<< cairoSurfaceWriteToPng sfc0 "HaskellLogoRotated.png"
	putStrLn "*** TEST ROTATE END ***"

