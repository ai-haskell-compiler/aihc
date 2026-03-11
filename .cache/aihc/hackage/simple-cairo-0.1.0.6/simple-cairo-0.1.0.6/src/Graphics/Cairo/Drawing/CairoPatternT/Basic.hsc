{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Basic where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Traversable
import Data.Maybe
import Data.Word
import Data.Color
import System.IO.Unsafe

import Graphics.Cairo.Exception
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal

#include <cairo.h>

class IsCairoPatternT pt where toCairoPatternT :: pt s -> CairoPatternT s

newtype CairoPatternT s = CairoPatternT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternT where toCairoPatternT = id

makeCairoPatternT :: Ptr (CairoPatternT s) -> IO (CairoPatternT s)
makeCairoPatternT p = CairoPatternT <$> newForeignPtr p (c_cairo_pattern_destroy p)

returnCairoPatternT :: PrimMonad m => IO (Ptr (CairoPatternT (PrimState m))) -> m (CairoPatternT (PrimState m))
returnCairoPatternT io = unsafeIOToPrim $ makeCairoPatternT =<< io

foreign import ccall "cairo_pattern_destroy" c_cairo_pattern_destroy ::
	Ptr (CairoPatternT s) -> IO ()

newtype CairoPatternTypeT = CairoPatternTypeT #{type cairo_pattern_type_t} deriving Show

pattern CairoPatternTypeSolid :: CairoPatternTypeT
pattern CairoPatternTypeSolid <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SOLID} where
	CairoPatternTypeSolid = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SOLID}

pattern CairoPatternTypeSurface :: CairoPatternTypeT
pattern CairoPatternTypeSurface <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SURFACE} where
	CairoPatternTypeSurface = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SURFACE}

pattern CairoPatternTypeLinear :: CairoPatternTypeT
pattern CairoPatternTypeLinear <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_LINEAR} where
	CairoPatternTypeLinear = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_LINEAR}

pattern CairoPatternTypeRadial :: CairoPatternTypeT
pattern CairoPatternTypeRadial <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RADIAL} where
	CairoPatternTypeRadial = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RADIAL}

pattern CairoPatternTypeMesh :: CairoPatternTypeT
pattern CairoPatternTypeMesh <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_MESH} where
	CairoPatternTypeMesh = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_MESH}

pattern CairoPatternTypeRasterSource :: CairoPatternTypeT
pattern CairoPatternTypeRasterSource <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RASTER_SOURCE} where
	CairoPatternTypeRasterSource = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RASTER_SOURCE}

cairoPatternGetType :: CairoPatternT s -> CairoPatternTypeT
cairoPatternGetType (CairoPatternT fpt) = unsafePerformIO $ withForeignPtr fpt \pt ->
	CairoPatternTypeT <$> c_cairo_pattern_get_type pt

foreign import ccall "cairo_pattern_get_type" c_cairo_pattern_get_type ::
	Ptr (CairoPatternT s) -> IO #{type cairo_pattern_type_t}

newtype CairoPatternSolidT s = CairoPatternSolidT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternSolidT where toCairoPatternT = CairoPatternTSolid

pattern CairoPatternTSolid :: CairoPatternSolidT s -> CairoPatternT s
pattern CairoPatternTSolid pts <- (cairoPatternSolidT -> Just pts) where
	CairoPatternTSolid (CairoPatternSolidT pts) = CairoPatternT pts

cairoPatternSolidT :: CairoPatternT s -> Maybe (CairoPatternSolidT s)
cairoPatternSolidT pt@(CairoPatternT fpt) = case cairoPatternGetType pt of
	CairoPatternTypeSolid -> Just $ CairoPatternSolidT fpt
	_ -> Nothing

cairoPatternCreateRgb :: PrimMonad m => Rgb CDouble -> m (CairoPatternSolidT (PrimState m))
cairoPatternCreateRgb (RgbDouble r g b) = unsafeIOToPrim do
	ppt <- c_cairo_pattern_create_rgb r g b
	pt <- CairoPatternSolidT <$> newForeignPtr ppt (c_cairo_pattern_destroy ppt)
	pt <$ raiseIfErrorPattern (CairoPatternTSolid pt)

foreign import ccall "cairo_pattern_create_rgb" c_cairo_pattern_create_rgb ::
	CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))

cairoPatternCreateRgba :: PrimMonad m => Rgba CDouble -> m (CairoPatternSolidT (PrimState m))
cairoPatternCreateRgba (RgbaDouble r g b a) = unsafeIOToPrim do
	ppt <- c_cairo_pattern_create_rgba r g b a
	pt <- CairoPatternSolidT <$> newForeignPtr ppt (c_cairo_pattern_destroy ppt)
	pt <$ raiseIfErrorPattern (CairoPatternTSolid pt)

foreign import ccall "cairo_pattern_create_rgba" c_cairo_pattern_create_rgba ::
	CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))

cairoPatternGetRgba :: PrimMonad m => CairoPatternSolidT (PrimState m) -> m (Rgba CDouble)
cairoPatternGetRgba (CairoPatternSolidT fpts) = unsafeIOToPrim $ withForeignPtr fpts \pts ->
	alloca \r -> alloca \g -> alloca \b -> alloca \a -> do
		cairoStatusToThrowError =<< c_cairo_pattern_get_rgba pts r g b a
		rgbaDouble <$> peek r <*> peek g <*> peek b <*> peek a >>= \case
			Just rgba -> pure rgba
			Nothing -> error $ "(r, g, b, a) = " ++ show (r, g, b, a)

foreign import ccall "cairo_pattern_get_rgba" c_cairo_pattern_get_rgba ::
	Ptr (CairoPatternT s) ->
	Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
	IO #{type cairo_status_t}

class IsCairoPatternT pt => IsCairoPatternGradientT pt where
	toCairoPatternGradientT :: pt s -> CairoPatternGradientT s

newtype CairoPatternGradientT s = CairoPatternGradientT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternGradientT where
	toCairoPatternT = CairoPatternTGradient

instance IsCairoPatternGradientT CairoPatternGradientT where
	toCairoPatternGradientT = id

pattern CairoPatternTGradient :: CairoPatternGradientT s -> CairoPatternT s
pattern CairoPatternTGradient ptg <- (cairoPatternGradientT -> Just ptg) where
	CairoPatternTGradient (CairoPatternGradientT ptg) = CairoPatternT ptg

cairoPatternGradientT :: CairoPatternT s -> Maybe (CairoPatternGradientT s)
cairoPatternGradientT pt@(CairoPatternT fpt) = case cairoPatternGetType pt of
	CairoPatternTypeLinear -> Just $ CairoPatternGradientT fpt
	CairoPatternTypeRadial -> Just $ CairoPatternGradientT fpt
	_ -> Nothing

cairoPatternAddColorStopRgb :: (PrimMonad m, IsCairoPatternGradientT pt) =>
	pt (PrimState m) -> CDouble -> Rgb CDouble -> m ()
cairoPatternAddColorStopRgb (toCairoPatternGradientT -> CairoPatternGradientT fpt) os (RgbDouble r g b) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> c_cairo_pattern_add_color_stop_rgb ppt os r g b

foreign import ccall "cairo_pattern_add_color_stop_rgb" c_cairo_pattern_add_color_stop_rgb ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoPatternAddColorStopRgba :: (PrimMonad m, IsCairoPatternGradientT pt) =>
	pt (PrimState m) -> CDouble -> Rgba CDouble -> m ()
cairoPatternAddColorStopRgba (toCairoPatternGradientT -> CairoPatternGradientT fpt) os (RgbaDouble r g b a) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> c_cairo_pattern_add_color_stop_rgba ppt os r g b a

foreign import ccall "cairo_pattern_add_color_stop_rgba" c_cairo_pattern_add_color_stop_rgba ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

newtype CairoPatternLinearT s = CairoPatternLinearT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternGradientT CairoPatternLinearT where
	toCairoPatternGradientT = CairoPatternGradientTLinear

instance IsCairoPatternT CairoPatternLinearT where
	toCairoPatternT = CairoPatternTGradient . toCairoPatternGradientT

pattern CairoPatternGradientTLinear :: CairoPatternLinearT s -> CairoPatternGradientT s
pattern CairoPatternGradientTLinear ptl <- (cairoPatternGradientLinearT -> Just ptl) where
	CairoPatternGradientTLinear (CairoPatternLinearT ptl) = CairoPatternGradientT ptl

cairoPatternGradientLinearT :: CairoPatternGradientT s -> Maybe (CairoPatternLinearT s)
cairoPatternGradientLinearT pt@(CairoPatternGradientT fpt) = case cairoPatternGetType $ CairoPatternTGradient pt of
	CairoPatternTypeLinear -> Just $ CairoPatternLinearT fpt
	_ -> Nothing

cairoPatternCreateLinear :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> m (CairoPatternLinearT (PrimState m))
cairoPatternCreateLinear x0 y0 x1 y1 = unsafeIOToPrim
	$ CairoPatternLinearT <$> do
		p <- c_cairo_pattern_create_linear x0 y0 x1 y1
		newForeignPtr p $ c_cairo_pattern_destroy p

foreign import ccall "cairo_pattern_create_linear" c_cairo_pattern_create_linear ::
	CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))

cairoPatternGetLinearPoints :: PrimMonad m =>
	CairoPatternLinearT (PrimState m) -> m ((CDouble, CDouble), (CDouble, CDouble))
cairoPatternGetLinearPoints (CairoPatternLinearT fpt) = unsafeIOToPrim
	$ withForeignPtr fpt \ppt -> alloca \x0 -> alloca \y0 -> alloca \x1 -> alloca \y1 -> do
		cs <- c_cairo_pattern_get_linear_points ppt x0 y0 x1 y1
		cairoStatusToThrowError cs
		(,) <$> ((,) <$> peek x0 <*> peek y0) <*> ((,) <$> peek x1 <*> peek y1)

foreign import ccall "cairo_pattern_get_linear_points" c_cairo_pattern_get_linear_points ::
	Ptr (CairoPatternT s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO #{type cairo_status_t}

cairoPatternGetColorStopRgbaList :: (PrimMonad m, IsCairoPatternGradientT pt) =>
	pt (PrimState m) -> m [(CDouble, Rgba CDouble)]
cairoPatternGetColorStopRgbaList pt = cairoPatternGetColorStopCount pt >>= \n ->
	for [0 .. n - 1] \i -> cairoPatternGetColorStopRgba pt i

cairoPatternGetColorStopCount :: (PrimMonad m, IsCairoPatternGradientT pt) =>
	pt (PrimState m) -> m CInt
cairoPatternGetColorStopCount (toCairoPatternGradientT -> CairoPatternGradientT fpt) = unsafeIOToPrim
	$ withForeignPtr fpt \ppt -> alloca \n -> do
		cs <- c_cairo_pattern_get_color_stop_count ppt n
		cairoStatusToThrowError cs
		peek n

foreign import ccall "cairo_pattern_get_color_stop_count" c_cairo_pattern_get_color_stop_count ::
	Ptr (CairoPatternT s) -> Ptr CInt -> IO #{type cairo_status_t}

cairoPatternGetColorStopRgba :: (PrimMonad m, IsCairoPatternGradientT pt) =>
	pt (PrimState m) -> CInt -> m (CDouble, Rgba CDouble)
cairoPatternGetColorStopRgba (toCairoPatternGradientT -> CairoPatternGradientT fpt) i = unsafeIOToPrim
	$ withForeignPtr fpt \ppt -> alloca \os -> alloca \rd -> alloca \gr -> alloca \bl -> alloca \al -> do
		cs <- c_cairo_pattern_get_color_stop_rgba ppt i os rd gr bl al
		cairoStatusToThrowError cs
		(,) <$> peek os <*> ((\r g b a -> fromJust $ rgbaDouble r g b a) <$> peek rd <*> peek gr <*> peek bl <*> peek al)

foreign import ccall "cairo_pattern_get_color_stop_rgba" c_cairo_pattern_get_color_stop_rgba ::
	Ptr (CairoPatternT s) -> CInt ->
	Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO #{type cairo_status_t}

newtype CairoPatternRadialT s = CairoPatternRadialT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternRadialT where
	toCairoPatternT = CairoPatternTGradient . toCairoPatternGradientT

instance IsCairoPatternGradientT CairoPatternRadialT where
	toCairoPatternGradientT = CairoPatternGradientTRadial

pattern CairoPatternGradientTRadial :: CairoPatternRadialT s -> CairoPatternGradientT s
pattern CairoPatternGradientTRadial pt <- (cairoPatternGradientRadialT -> Just pt) where
	CairoPatternGradientTRadial (CairoPatternRadialT fpt) = CairoPatternGradientT fpt

cairoPatternGradientRadialT :: CairoPatternGradientT s -> Maybe (CairoPatternRadialT s)
cairoPatternGradientRadialT pt@(CairoPatternGradientT fpt) = case cairoPatternGetType $ CairoPatternTGradient pt of
	CairoPatternTypeRadial -> Just $ CairoPatternRadialT fpt
	_ -> Nothing

cairoPatternCreateRadial :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble ->
	m (CairoPatternRadialT (PrimState m))
cairoPatternCreateRadial cx0 cy0 r0 cx1 cy1 r1 = unsafeIOToPrim
	$ CairoPatternRadialT <$> do
		p <- c_cairo_pattern_create_radial cx0 cy0 r0 cx1 cy1 r1
		newForeignPtr p $ c_cairo_pattern_destroy p

foreign import ccall "cairo_pattern_create_radial" c_cairo_pattern_create_radial ::
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))

cairoPatternGetRadialCircles :: PrimMonad m =>
	CairoPatternRadialT (PrimState m) -> m ((CDouble, CDouble, CDouble), (CDouble, CDouble, CDouble))
cairoPatternGetRadialCircles (CairoPatternRadialT fpt) = unsafeIOToPrim
	$ withForeignPtr fpt \ppt -> alloca \x0 -> alloca \y0 -> alloca \r0 -> alloca \x1 -> alloca \y1 -> alloca \r1 -> do
		cs <- c_cairo_pattern_get_radial_circles ppt x0 y0 r0 x1 y1 r1
		cairoStatusToThrowError cs
		(,) <$> ((,,) <$> peek x0 <*> peek y0 <*> peek r0) <*> ((,,) <$> peek x1 <*> peek y1 <*> peek r1)

foreign import ccall "cairo_pattern_get_radial_circles" c_cairo_pattern_get_radial_circles ::
	Ptr (CairoPatternT s) ->
	Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO #{type cairo_status_t}

newtype CairoPatternSurfaceT s = CairoPatternSurfaceT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternSurfaceT where
	toCairoPatternT = CairoPatternTSurface

pattern CairoPatternTSurface :: CairoPatternSurfaceT s -> CairoPatternT s
pattern CairoPatternTSurface pt <- (cairoPatternTSurface -> Just pt) where
	CairoPatternTSurface (CairoPatternSurfaceT fpt) = CairoPatternT fpt

cairoPatternTSurface :: CairoPatternT s -> Maybe (CairoPatternSurfaceT s)
cairoPatternTSurface pt@(CairoPatternT fpt) = case cairoPatternGetType pt of
	CairoPatternTypeSurface -> Just $ CairoPatternSurfaceT fpt
	_ -> Nothing

cairoPatternCreateForSurface :: (PrimMonad m, IsCairoSurfaceT sr) =>
	sr s (PrimState m) -> m (CairoPatternSurfaceT (PrimState m))
cairoPatternCreateForSurface (toCairoSurfaceT -> CairoSurfaceT fs) =
	unsafeIOToPrim $ CairoPatternSurfaceT <$> withForeignPtr fs \ps -> do
		p <- c_cairo_pattern_create_for_surface ps
		newForeignPtr p $ touchForeignPtr fs >> c_cairo_pattern_destroy p

foreign import ccall "cairo_pattern_create_for_surface" c_cairo_pattern_create_for_surface ::
	Ptr (CairoSurfaceT s ps) -> IO (Ptr (CairoPatternT ps))

cairoPatternGetSurface :: PrimMonad m =>
	CairoPatternSurfaceT (PrimState m) -> m (CairoSurfaceT s (PrimState m))
cairoPatternGetSurface (CairoPatternSurfaceT fpt) =
	unsafeIOToPrim $ CairoSurfaceT <$> withForeignPtr fpt \ppt -> alloca \pps -> do
		cairoStatusToThrowError =<< c_cairo_pattern_get_surface ppt pps
		p <- c_cairo_surface_reference =<< peek pps
		newForeignPtr p $ c_cairo_surface_destroy p

foreign import ccall "cairo_pattern_get_surface" c_cairo_pattern_get_surface ::
	Ptr (CairoPatternT ps) -> Ptr (Ptr (CairoSurfaceT s ps)) -> IO #{type cairo_status_t}

foreign import ccall "cairo_surface_reference" c_cairo_surface_reference ::
	Ptr (CairoSurfaceT s ps) -> IO (Ptr (CairoSurfaceT s ps))

raiseIfErrorPattern :: CairoPatternT s -> IO ()
raiseIfErrorPattern (CairoPatternT fpt) = withForeignPtr fpt \pt ->
	cairoStatusToThrowError =<< c_cairo_pattern_status pt

foreign import ccall "cairo_pattern_status" c_cairo_pattern_status ::
	Ptr (CairoPatternT s) -> IO #{type cairo_status_t}
