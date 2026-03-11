{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT (
	-- * Basic
	cairoCreate,
	-- * Push and Pop Group
	cairoPushGroup, cairoPopGroup, cairoPopGroupToSource,
	-- * Set Source
	cairoSetSourceRgb, cairoSetSourceRgba, cairoSetSource, cairoSetSourceSurface,
	-- * Set Attribute
	cairoSetLineWidth,
	-- * Verb
	cairoFill, cairoMask, cairoPaint, cairoPaintWithAlpha,
	cairoStroke, cairoStrokePreserve, cairoStrokeExtents,
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Exception

import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Drawing.CairoT.Setting

import Data.CairoContext

import Graphics.Cairo.Drawing.CairoPatternT.Basic

foreign import ccall "cairo_push_group" c_cairo_push_group ::
	Ptr (CairoT r s) -> IO ()

cairoPushGroup :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoPushGroup = (`withCairoT` c_cairo_push_group)

foreign import ccall "cairo_pop_group_to_source" c_cairo_pop_group_to_source ::
	Ptr (CairoT r s) -> IO ()

cairoPopGroupToSource :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoPopGroupToSource cr@(CairoT fcr) = unsafeIOToPrim $ withForeignPtr fcr c_cairo_pop_group_to_source <* raiseIfError cr

foreign import ccall "cairo_pop_group" c_cairo_pop_group ::
	Ptr (CairoT r s) -> IO (Ptr (CairoPatternT s))

cairoPopGroup :: PrimMonad m => CairoT r s -> m (CairoPatternT s)
cairoPopGroup (CairoT fcr) = unsafeIOToPrim $ makeCairoPatternT =<< withForeignPtr fcr c_cairo_pop_group
