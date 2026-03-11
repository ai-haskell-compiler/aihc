{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths.Relative where

import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.Primitive
import Data.CairoContext
import Graphics.Cairo.Exception

cairoRelMoveTo :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m ()
cairoRelMoveTo cr dx dy = do
	withCairoT cr \pcr -> c_cairo_rel_move_to pcr dx dy
	unsafeIOToPrim $ raiseIfError cr

foreign import ccall "cairo_rel_move_to" c_cairo_rel_move_to ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO ()

cairoRelLineTo :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m ()
cairoRelLineTo cr x y = do
	withCairoT cr \pcr -> c_cairo_rel_line_to pcr x y
	unsafeIOToPrim $ raiseIfError cr

foreign import ccall "cairo_rel_line_to" c_cairo_rel_line_to ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO ()

cairoRelCurveTo :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble ->
	CDouble -> CDouble ->
	CDouble -> CDouble -> m ()
cairoRelCurveTo cr dx1 dy1 dx2 dy2 dx3 dy3 = do
	withCairoT cr \pcr -> c_cairo_rel_curve_to pcr dx1 dy1 dx2 dy2 dx3 dy3
	unsafeIOToPrim $ raiseIfError cr

foreign import ccall "cairo_rel_curve_to" c_cairo_rel_curve_to ::
	Ptr (CairoT r s) -> CDouble -> CDouble ->
		CDouble -> CDouble ->
		CDouble -> CDouble -> IO ()
