{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Basic (
	cairoCreate,
	cairoSetSourceRgb, cairoSetSourceRgba, cairoSetSource, cairoSetSourceSurface,
	cairoStroke, cairoStrokePreserve, cairoStrokeExtents, cairoInStroke,
	cairoFill, cairoFillPreserve, cairoFillExtents, cairoInFill,
	cairoPaint, cairoPaintWithAlpha,
	cairoMask, cairoMaskSurface,

	CairoExtents(..),
	pattern CairoExtentsLeftTopWidthHeight, cairoExtentsLeft, cairoExtentsTop, cairoExtentsWidth, cairoExtentsHeight
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Control.Concurrent.STM
import Data.Int

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Exception
import Graphics.Cairo.Drawing.Extents

import Data.Color
import Data.CairoContext

import Graphics.Cairo.Drawing.CairoPatternT.Basic

#include <cairo.h>

cairoCreate :: (PrimMonad m, IsCairoSurfaceT sr) =>
	sr s (PrimState m) -> m (CairoT r (PrimState m))
cairoCreate sr_ = unsafeIOToPrim do
	cr <- withForeignPtr sr c_cairo_create >>= \pcr ->
		CairoT <$> newForeignPtr pcr (do
			atomically (readTChan =<< ck)
			putStrLn "begin CairoT destroy"
			c_cairo_destroy pcr
			putStrLn "end CairoT destroy")
	cr <$ raiseIfError cr
	where
	CairoSurfaceT sr = toCairoSurfaceT sr_
	ck = cairoSurfaceTFinishChecker sr_

foreign import ccall "cairo_create"
	c_cairo_create :: Ptr (CairoSurfaceT s ps) -> IO (Ptr (CairoT r ps))
foreign import ccall "cairo_destroy" c_cairo_destroy :: Ptr (CairoT r s) -> IO ()

cairoSetSourceRgb :: PrimMonad m => CairoT r (PrimState m) -> Rgb CDouble -> m ()
cairoSetSourceRgb cr (RgbDouble r g b) =
	withCairoT cr \pcr -> c_cairo_set_source_rgb pcr r g b

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> CDouble -> IO ()

cairoSetSourceRgba :: PrimMonad m => CairoT r (PrimState m) -> Rgba CDouble -> m ()
cairoSetSourceRgba cr (RgbaDouble r g b a) =
	withCairoT cr \pcr -> c_cairo_set_source_rgba pcr r g b a

foreign import ccall "cairo_set_source_rgba" c_cairo_set_source_rgba ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoSetSource :: (PrimMonad m, IsCairoPatternT pt) => CairoT r s -> pt s -> m ()
cairoSetSource cr@(CairoT fcr) (toCairoPatternT -> CairoPatternT fpt) = unsafeIOToPrim do
	withForeignPtr fcr \pcr -> withForeignPtr fpt \ppt -> c_cairo_set_source pcr ppt >> raiseIfError cr
	addForeignPtrFinalizer fcr $ touchForeignPtr fpt

foreign import ccall "cairo_set_source" c_cairo_set_source ::
	Ptr (CairoT r s) -> Ptr (CairoPatternT s) -> IO ()

cairoSetSourceSurface :: (PrimMonad m, IsCairoSurfaceT sr) =>
	CairoT r ps -> sr s ps -> CDouble -> CDouble -> m ()
cairoSetSourceSurface (CairoT fcr) (toCairoSurfaceT -> CairoSurfaceT fsr) x y = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fsr \sr -> c_cairo_set_source_surface cr sr x y

foreign import ccall "cairo_set_source_surface" c_cairo_set_source_surface ::
	Ptr (CairoT r ps) -> Ptr (CairoSurfaceT s ps) -> CDouble -> CDouble -> IO ()

cairoStroke :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoStroke = (`withCairoT` c_cairo_stroke)

foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr (CairoT r s) -> IO ()

cairoStrokePreserve :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoStrokePreserve = (`withCairoT` c_cairo_stroke_preserve)

foreign import ccall "cairo_stroke_preserve" c_cairo_stroke_preserve :: Ptr (CairoT r s) -> IO ()

cairoStrokeExtents :: PrimMonad m => CairoT r (PrimState m) -> m CairoExtents
cairoStrokeExtents = flip withCairoT \pcr -> alloca \x1 -> alloca \y1 -> alloca \x2 -> alloca \y2 -> do
	c_cairo_stroke_extents pcr x1 y1 x2 y2
	CairoExtentsLeftTopRightBottom <$> peek x1 <*> peek y1 <*> peek x2 <*> peek y2

foreign import ccall "cairo_stroke_extents" c_cairo_stroke_extents ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoInStroke :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m Bool
cairoInStroke cr x y = withCairoT cr \pcr -> (/= 0) <$> c_cairo_in_stroke pcr x y

foreign import ccall "cairo_in_stroke" c_cairo_in_stroke ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO #{type cairo_bool_t}

cairoFill :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoFill = (`withCairoT` c_cairo_fill)

foreign import ccall "cairo_fill" c_cairo_fill :: Ptr (CairoT r s) -> IO ()

cairoFillPreserve :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoFillPreserve = (`withCairoT` c_cairo_fill_preserve)

foreign import ccall "cairo_fill_preserve" c_cairo_fill_preserve :: Ptr (CairoT r s) -> IO ()

cairoFillExtents :: PrimMonad m => CairoT r (PrimState m) -> m CairoExtents
cairoFillExtents = flip withCairoT \pcr -> alloca \x1 -> alloca \y1 -> alloca \x2 -> alloca \y2 -> do
	c_cairo_fill_extents pcr x1 y1 x2 y2
	CairoExtentsLeftTopRightBottom <$> peek x1 <*> peek y1 <*> peek x2 <*> peek y2

foreign import ccall "cairo_fill_extents" c_cairo_fill_extents ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoInFill :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m Bool
cairoInFill cr x y = withCairoT cr \pcr -> (/= 0) <$> c_cairo_in_fill pcr x y

foreign import ccall "cairo_in_fill" c_cairo_in_fill ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO #{type cairo_bool_t}

cairoPaint :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoPaint cr = withCairoT cr \pcr -> c_cairo_paint pcr >> raiseIfError cr

foreign import ccall "cairo_paint" c_cairo_paint :: Ptr (CairoT r s) -> IO ()

cairoPaintWithAlpha :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> m ()
cairoPaintWithAlpha cr a = withCairoT cr \pcr -> c_cairo_paint_with_alpha pcr a

foreign import ccall "cairo_paint_with_alpha" c_cairo_paint_with_alpha ::
	Ptr (CairoT r s) -> CDouble -> IO ()

cairoMask :: (PrimMonad m, IsCairoPatternT pt) => CairoT r s -> pt s -> m ()
cairoMask cr@(CairoT fcr) (toCairoPatternT -> CairoPatternT fpt) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withForeignPtr fpt (c_cairo_mask pcr) >> raiseIfError cr

foreign import ccall "cairo_mask" c_cairo_mask ::
	Ptr (CairoT r s) -> Ptr (CairoPatternT s) -> IO ()

cairoMaskSurface :: (PrimMonad m, IsCairoSurfaceT sr) => CairoT r ps -> sr s ps -> CDouble -> CDouble -> m ()
cairoMaskSurface (CairoT fcr) (toCairoSurfaceT -> CairoSurfaceT fsr) x y = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fsr \sr -> c_cairo_mask_surface cr sr x y

foreign import ccall "cairo_mask_surface" c_cairo_mask_surface ::
	Ptr (CairoT r ps) -> Ptr (CairoSurfaceT s ps) -> CDouble -> CDouble -> IO ()
