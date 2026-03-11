{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths.Basic where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Angle
import Data.CairoContext

import Graphics.Cairo.Drawing.Extents

cairoNewPath :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoNewPath = (`withCairoT` c_cairo_new_path)

foreign import ccall "cairo_new_path" c_cairo_new_path :: Ptr (CairoT r s) -> IO ()

cairoNewSubPath :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoNewSubPath = (`withCairoT` c_cairo_new_sub_path)

foreign import ccall "cairo_new_sub_path" c_cairo_new_sub_path :: Ptr (CairoT r s) -> IO ()

cairoMoveTo :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m ()
cairoMoveTo cr x y = withCairoT cr \pcr -> c_cairo_move_to pcr x y

foreign import ccall "cairo_move_to" c_cairo_move_to ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO ()

cairoLineTo :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m ()
cairoLineTo cr x y = withCairoT cr \pcr -> c_cairo_line_to pcr x y

foreign import ccall "cairo_line_to" c_cairo_line_to ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO ()

cairoCurveTo :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble ->
	CDouble -> CDouble -> CDouble -> CDouble -> m ()
cairoCurveTo cr x1 y1 x2 y2 x3 y3 = withCairoT cr \pcr -> c_cairo_curve_to pcr x1 y1 x2 y2 x3 y3

foreign import ccall "cairo_curve_to" c_cairo_curve_to ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoClosePath :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoClosePath = (`withCairoT` c_cairo_close_path)

foreign import ccall "cairo_close_path" c_cairo_close_path ::
	Ptr (CairoT r s) -> IO ()

cairoRectangle :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> CDouble -> CDouble -> m ()
cairoRectangle cr x y w h = withCairoT cr \pcr -> c_cairo_rectangle pcr x y w h

foreign import ccall "cairo_rectangle" c_cairo_rectangle ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoArc :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble ->
	CDouble -> Angle CDouble -> Angle CDouble -> m ()
cairoArc cr xc yc r (Radian a1) (Radian a2) =
	withCairoT cr \pcr -> c_cairo_arc pcr xc yc r a1 a2

foreign import ccall "cairo_arc" c_cairo_arc :: Ptr (CairoT r s) ->
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoArcNegative :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble ->
	CDouble -> Angle CDouble -> Angle CDouble -> m ()
cairoArcNegative cr xc yc r (Radian a1) (Radian a2) =
	withCairoT cr \pcr -> c_cairo_arc_negative pcr xc yc r a1 a2

foreign import ccall "cairo_arc_negative" c_cairo_arc_negative ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble ->
	IO ()

cairoPathExtents :: PrimMonad m => CairoT r (PrimState m) -> m CairoExtents
cairoPathExtents = flip withCairoT \pcr -> alloca \x1 -> alloca \y1 -> alloca \x2 -> alloca \y2 -> do
	c_cairo_path_extents pcr x1 y1 x2 y2
	CairoExtentsLeftTopRightBottom <$> peek x1 <*> peek y1 <*> peek x2 <*> peek y2

foreign import ccall "cairo_path_extents" c_cairo_path_extents ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
