{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.PngSupport (
	cairoSurfaceCreateFromPng, cairoSurfaceCreateFromPngByteString,
	cairoSurfaceWriteToPng
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.C
import Control.Monad.Primitive
import Control.Concurrent.STM
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Exception

import Graphics.Cairo.Surfaces.ImageSurfaces

#include <cairo.h>

foreign import ccall "cairo_surface_write_to_png" c_cairo_surface_write_to_png ::
	Ptr (CairoSurfaceT s ps) -> CString -> IO #type cairo_status_t

cairoSurfaceWriteToPng :: IsCairoSurfaceT sfc => sfc s ps -> FilePath -> IO CairoStatusT
cairoSurfaceWriteToPng (toCairoSurfaceT -> CairoSurfaceT s) fp = withCString fp \cs -> CairoStatusT
	<$> (withForeignPtr s \p -> c_cairo_surface_write_to_png p cs)

foreign import ccall "cairo_image_surface_create_from_png" c_cairo_surface_create_from_png ::
	CString -> IO (Ptr (CairoSurfaceT s ps))

cairoSurfaceCreateFromPng :: FilePath -> IO (CairoSurfaceImageT s ps)
cairoSurfaceCreateFromPng fp = withCString fp \cs ->
	mkCairoSurfaceImageT =<< c_cairo_surface_create_from_png cs

cairoSurfaceCreateFromPngByteString :: PrimMonad m =>
	BS.ByteString -> m (CairoSurfaceImageT s (PrimState m))
cairoSurfaceCreateFromPngByteString bs = unsafeIOToPrim do
	tbs <- atomically $ newTVar bs
	fn <- c_cairo_read_func_t $ byteStringToCCairoReadFunc tbs
	mkCairoSurfaceImageT
		=<< c_cairo_image_surface_create_from_png_stream fn nullPtr

foreign import ccall "cairo_image_surface_create_from_png_stream"
	c_cairo_image_surface_create_from_png_stream ::
	FunPtr (CCairoReadFunc a) -> Ptr a -> IO (Ptr (CairoSurfaceT s ps))

type CCairoReadFunc a = Ptr a -> CString -> #{type unsigned int} -> IO #{type cairo_status_t}

foreign import ccall "wrapper" c_cairo_read_func_t ::
	CCairoReadFunc a -> IO (FunPtr (CCairoReadFunc a))

byteStringToCCairoReadFunc :: TVar BS.ByteString -> CCairoReadFunc a
byteStringToCCairoReadFunc tbs _cls dt ln = do
	bs <- atomically $ readTVar tbs
	if BS.length bs < (fromIntegral ln) then
		pure #{const CAIRO_STATUS_READ_ERROR} else do
		tx' <- atomically do
			let	(tx, bs') = BS.splitAt (fromIntegral ln) bs
			writeTVar tbs bs'
			pure tx
		let	(fptr, ln') = BS.toForeignPtr0 tx'
		withForeignPtr fptr \ptr -> do
			copyBytes dt (castPtr ptr) ln'
			pure #{const CAIRO_STATUS_SUCCESS}
