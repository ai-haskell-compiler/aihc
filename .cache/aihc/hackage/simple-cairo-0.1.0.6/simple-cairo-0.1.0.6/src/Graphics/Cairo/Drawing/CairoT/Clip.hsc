{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Clip where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Data.Int
import Data.CairoContext
import Graphics.Cairo.Drawing.Extents

#include <cairo.h>

cairoClip, cairoClipPreserve :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoClip = (`withCairoT` c_cairo_clip)
cairoClipPreserve = (`withCairoT` c_cairo_clip_preserve)

foreign import ccall "cairo_clip" c_cairo_clip :: Ptr (CairoT r s) -> IO ()

foreign import ccall "cairo_clip_preserve" c_cairo_clip_preserve :: Ptr (CairoT r s) -> IO ()

cairoClipExtents :: PrimMonad m => CairoT r (PrimState m) -> m CairoExtents
cairoClipExtents = flip withCairoT \pcr -> alloca \x1 -> alloca \y1 -> alloca \x2 -> alloca \y2 -> do
	c_cairo_clip_extents pcr x1 y1 x2 y2
	CairoExtentsLeftTopRightBottom <$> peek x1 <*> peek y1 <*> peek x2 <*> peek y2

foreign import ccall "cairo_clip_extents" c_cairo_clip_extents ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoInClip :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m Bool
cairoInClip cr x y = withCairoT cr \pcr -> (/= 0) <$> c_cairo_in_clip pcr x y

foreign import ccall "cairo_in_clip" c_cairo_in_clip ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO #{type cairo_bool_t}

cairoResetClip :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoResetClip = (`withCairoT` c_cairo_reset_clip)

foreign import ccall "cairo_reset_clip" c_cairo_reset_clip ::
	Ptr (CairoT r s) -> IO ()
