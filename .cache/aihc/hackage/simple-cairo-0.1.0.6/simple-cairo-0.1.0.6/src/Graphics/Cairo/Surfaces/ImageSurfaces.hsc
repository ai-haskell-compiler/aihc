{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.ImageSurfaces (
	CairoSurfaceImageT,
	pattern CairoSurfaceTImage,

	cairoImageSurfaceCreate,

	cairoImageSurfaceCreateForCairoImage,
	cairoImageSurfaceCreateForCairoImageMut,

	cairoImageSurfaceGetCairoImage,
	cairoImageSurfaceGetCairoImageMut,

	cairoImageSurfaceGetWidth,
	cairoImageSurfaceGetHeight,
	cairoImageSurfaceGetStride,

	mkCairoSurfaceImageT
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Data.Word
import Data.Int

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT

import Data.CairoImage.Internal hiding (Argb32, pixelAt, Image, Pixel)

#include <cairo.h>

newtype CairoSurfaceImageT s ps = CairoSurfaceImageT (ForeignPtr (CairoSurfaceT s ps)) deriving Show

pattern CairoSurfaceTImage :: CairoSurfaceImageT s ps -> CairoSurfaceT s ps
pattern CairoSurfaceTImage sr <- (cairoSurfaceTImage -> Just sr) where
	CairoSurfaceTImage = toCairoSurfaceT

cairoSurfaceTImage :: CairoSurfaceT s ps -> Maybe (CairoSurfaceImageT s ps)
cairoSurfaceTImage sr@(CairoSurfaceT fsr) = case cairoSurfaceGetType sr of
	CairoSurfaceTypeImage -> Just $ CairoSurfaceImageT fsr
	_ -> Nothing

instance IsCairoSurfaceT CairoSurfaceImageT where
	toCairoSurfaceT (CairoSurfaceImageT fsr) = CairoSurfaceT fsr

cairoImageSurfaceCreate :: PrimMonad m => CairoFormatT -> #{type int} -> #{type int} -> m (CairoSurfaceImageT s (PrimState m))
cairoImageSurfaceCreate (CairoFormatT f) w h =
	returnCairoSurfaceT $ c_cairo_image_surface_create f w h

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s ps))

foreign import ccall "cairo_image_surface_get_format" c_cairo_image_surface_get_format ::
	Ptr (CairoSurfaceT s ps) -> IO #type cairo_format_t

foreign import ccall "cairo_image_surface_get_stride" c_cairo_image_surface_get_stride ::
	Ptr (CairoSurfaceT s ps) -> IO #type int

foreign import ccall "cairo_image_surface_get_width" c_cairo_image_surface_get_width ::
	Ptr (CairoSurfaceT s ps) -> IO #type int

foreign import ccall "cairo_image_surface_get_height" c_cairo_image_surface_get_height ::
	Ptr (CairoSurfaceT s ps) -> IO #type int

cairoImageSurfaceGetCairoImage :: PrimMonad m => CairoSurfaceImageT s (PrimState m) -> m CairoImage
cairoImageSurfaceGetCairoImage = argCairoSurfaceT \sfc -> do
	d <- c_cairo_image_surface_get_data sfc
	f <- c_cairo_image_surface_get_format sfc
	w <- c_cairo_image_surface_get_width sfc
	h <- c_cairo_image_surface_get_height sfc
	s <- c_cairo_image_surface_get_stride sfc
	p <- mallocBytes . fromIntegral $ s * h
	copyBytes p d . fromIntegral $ s * h
	fd <- newForeignPtr (castPtr p) $ free (castPtr p)
	pure $ CairoImage (CairoFormatT f) (fromIntegral w) (fromIntegral h) (fromIntegral s) fd

cairoImageSurfaceGetCairoImageMut :: PrimMonad m => CairoSurfaceImageT s (PrimState m) -> m (CairoImageMut (PrimState m))
cairoImageSurfaceGetCairoImageMut = argCairoSurfaceT \sfc -> do
	d <- c_cairo_image_surface_get_data sfc
	f <- c_cairo_image_surface_get_format sfc
	w <- c_cairo_image_surface_get_width sfc
	h <- c_cairo_image_surface_get_height sfc
	s <- c_cairo_image_surface_get_stride sfc
	p <- mallocBytes . fromIntegral $ s * h
	copyBytes p d . fromIntegral $ s * h
	fd <- newForeignPtr (castPtr p) $ free (castPtr p)
	pure $ CairoImageMut (CairoFormatT f) (fromIntegral w) (fromIntegral h) (fromIntegral s) fd

newtype Argb32 = Argb32 Word32 deriving (Show, Storable)

foreign import ccall "cairo_image_surface_create_for_data" c_cairo_image_surface_create_for_data ::
	Ptr #{type unsigned char} -> #{type cairo_format_t} -> #{type int} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s ps))

cairoImageSurfaceCreateForCairoImage ::
	PrimMonad m => CairoImage -> m (CairoSurfaceImageT s (PrimState m))
cairoImageSurfaceCreateForCairoImage (CairoImage (CairoFormatT f) w h s d) = unsafeIOToPrim do
	p <- mallocBytes n
	withForeignPtr d \pd -> copyBytes p pd n
	sp <- c_cairo_image_surface_create_for_data (castPtr p) f (fromIntegral w) (fromIntegral h) (fromIntegral s)
	mkCairoSurfaceImageT' sp p
	where n = fromIntegral $ s * h

cairoImageSurfaceCreateForCairoImageMut ::
	PrimMonad m => CairoImageMut (PrimState m) -> m (CairoSurfaceImageT s (PrimState m))
cairoImageSurfaceCreateForCairoImageMut (CairoImageMut (CairoFormatT f) w h s d) = unsafeIOToPrim do
	p <- mallocBytes n
	withForeignPtr d \pd -> copyBytes p pd n
	sp <- c_cairo_image_surface_create_for_data (castPtr p) f (fromIntegral w) (fromIntegral h) (fromIntegral s)
	mkCairoSurfaceImageT' sp p
	where n = fromIntegral $ s * h

foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
	Ptr (CairoSurfaceT s ps) -> IO (Ptr #{type unsigned char})

argCairoSurfaceT :: PrimMonad m => (Ptr (CairoSurfaceT s (PrimState m)) -> IO a) -> CairoSurfaceImageT s (PrimState m) -> m a
argCairoSurfaceT io (CairoSurfaceImageT fs) = unsafeIOToPrim $ withForeignPtr fs io

returnCairoSurfaceT :: PrimMonad m => IO (Ptr (CairoSurfaceT s (PrimState m))) -> m (CairoSurfaceImageT s (PrimState m))
returnCairoSurfaceT io = unsafeIOToPrim $ mkCairoSurfaceImageT =<< io

mkCairoSurfaceImageT :: Ptr (CairoSurfaceT s ps) -> IO (CairoSurfaceImageT s ps)
mkCairoSurfaceImageT p = CairoSurfaceImageT <$> newForeignPtr p (c_cairo_surface_destroy p)

mkCairoSurfaceImageT' :: Ptr (CairoSurfaceT s ps) -> Ptr a -> IO (CairoSurfaceImageT s ps)
mkCairoSurfaceImageT' ps p = CairoSurfaceImageT <$> newForeignPtr ps (free p >> c_cairo_surface_destroy ps)

cairoImageSurfaceGetWidth :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> m #{type int}
cairoImageSurfaceGetWidth = argCairoSurfaceT \sfc ->
	c_cairo_image_surface_get_width sfc

cairoImageSurfaceGetHeight :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> m #{type int}
cairoImageSurfaceGetHeight = argCairoSurfaceT \sfc ->
	c_cairo_image_surface_get_height sfc

cairoImageSurfaceGetStride :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> m #{type int}
cairoImageSurfaceGetStride = argCairoSurfaceT \sfc ->
	c_cairo_image_surface_get_stride sfc
