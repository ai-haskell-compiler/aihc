{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Mesh where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad
import Control.Monad.Primitive
import Data.Word
import Data.Maybe
import Data.Color

import Graphics.Cairo.Exception
import Graphics.Cairo.Drawing.Paths.CairoPathT

#include <cairo.h>

import Graphics.Cairo.Drawing.CairoPatternT.Basic

newtype CairoPatternMeshT s = CairoPatternMeshT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternMeshT where
	toCairoPatternT = CairoPatternTMesh

pattern CairoPatternTMesh :: CairoPatternMeshT s -> CairoPatternT s
pattern CairoPatternTMesh pt <- (cairoPatternMeshT -> Just pt) where
	CairoPatternTMesh (CairoPatternMeshT fpt) = CairoPatternT fpt

cairoPatternMeshT :: CairoPatternT s -> Maybe (CairoPatternMeshT s)
cairoPatternMeshT pt@(CairoPatternT fpt) = case cairoPatternGetType pt of
	CairoPatternTypeMesh -> Just $ CairoPatternMeshT fpt
	_ -> Nothing

cairoPatternCreateMesh :: PrimMonad m => m (CairoPatternMeshT (PrimState m))
cairoPatternCreateMesh = unsafeIOToPrim do
	p <- c_cairo_pattern_create_mesh
	CairoPatternMeshT <$> newForeignPtr p (c_cairo_pattern_destroy p)

foreign import ccall "cairo_pattern_create_mesh" c_cairo_pattern_create_mesh ::
	IO (Ptr (CairoPatternT s))

data Color = ColorRgb (Rgb CDouble) | ColorRgba (Rgba CDouble) deriving Show

data Point = Point CDouble CDouble deriving Show

cairoMeshPatternAddPatchDefaultControlPoints3 :: PrimMonad m => CairoPatternMeshT (PrimState m) ->
	MoveTo -> LineCurveTo -> LineCurveTo -> CloseTo ->
	Color -> Color -> Color -> m ()
cairoMeshPatternAddPatchDefaultControlPoints3 pt mv lc1 lc2 cls c0 c1 c2 = do
	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt mv
	cairoMeshPatternLineTo pt `mapM_` [lc1, lc2]
	cairoMeshPatternCloseTo pt mv cls
	zipWithM_ (cairoMeshPatternSetCornerColor pt) [0 ..] [c0, c1, c2]
	cairoMeshPatternEndPatch pt

cairoMeshPatternAddPatchDefaultControlPoints :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) ->
	MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo ->
	Color -> Color -> Color -> Color -> m ()
cairoMeshPatternAddPatchDefaultControlPoints pt mv lc1 lc2 lc3 cls c0 c1 c2 c3 = do
	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt mv
	cairoMeshPatternLineTo pt `mapM_` [lc1, lc2, lc3]
	cairoMeshPatternCloseTo pt mv cls
	zipWithM_ (cairoMeshPatternSetCornerColor pt) [0 ..] [c0, c1, c2, c3]
	cairoMeshPatternEndPatch pt

cairoMeshPatternAddPatch :: PrimMonad m => CairoPatternMeshT (PrimState m) ->
	MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo ->
	Color -> Color -> Color -> Color ->
	Maybe Point -> Maybe Point -> Maybe Point -> Maybe Point -> m ()
cairoMeshPatternAddPatch pt mv lc1 lc2 lc3 cls c0 c1 c2 c3 cp0 cp1 cp2 cp3 = do
	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt mv
	cairoMeshPatternLineTo pt `mapM_` [lc1, lc2, lc3]
	cairoMeshPatternCloseTo pt mv cls
	zipWithM_ (cairoMeshPatternSetCornerColor pt) [0 ..] [c0, c1, c2, c3]
	zipWithM_ (cairoMeshPatternSetControlPointMaybe pt) [0 ..] [cp0, cp1, cp2, cp3]
	cairoMeshPatternEndPatch pt

cairoMeshPatternBeginPatch :: PrimMonad m => CairoPatternMeshT (PrimState m) -> m ()
cairoMeshPatternBeginPatch (CairoPatternMeshT fpt) = unsafeIOToPrim 
	$ withForeignPtr fpt c_cairo_mesh_pattern_begin_patch

foreign import ccall "cairo_mesh_pattern_begin_patch" c_cairo_mesh_pattern_begin_patch ::
	Ptr (CairoPatternT s) -> IO ()

cairoMeshPatternEndPatch :: PrimMonad m => CairoPatternMeshT (PrimState m) -> m ()
cairoMeshPatternEndPatch (CairoPatternMeshT fpt) = unsafeIOToPrim
	$ withForeignPtr fpt c_cairo_mesh_pattern_end_patch

foreign import ccall "cairo_mesh_pattern_end_patch" c_cairo_mesh_pattern_end_patch ::
	Ptr (CairoPatternT s) -> IO ()

cairoMeshPatternMoveTo :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> MoveTo -> m ()
cairoMeshPatternMoveTo (CairoPatternMeshT fpt) (MoveTo x y) = unsafeIOToPrim
	$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_move_to ppt x y

foreign import ccall "cairo_mesh_pattern_move_to" c_cairo_mesh_pattern_move_to ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> IO ()

cairoMeshPatternLineTo :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> LineCurveTo -> m ()
cairoMeshPatternLineTo (CairoPatternMeshT fpt) = \case
	LineTo x y -> unsafeIOToPrim
		$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_line_to ppt x y
	CurveTo x1  y1 x2 y2 x3 y3 -> unsafeIOToPrim
		$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_curve_to ppt x1 y1 x2 y2 x3 y3

cairoMeshPatternCloseTo :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> MoveTo -> CloseTo -> m ()
cairoMeshPatternCloseTo (CairoPatternMeshT fpt) (MoveTo x0 y0) cls =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> case cls of
		CloseLineTo -> pure ()
		CloseCurveTo x1 y1 x2 y2 -> c_cairo_mesh_pattern_curve_to ppt x1 y1 x2 y2 x0 y0

foreign import ccall "cairo_mesh_pattern_line_to" c_cairo_mesh_pattern_line_to ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> IO ()

foreign import ccall "cairo_mesh_pattern_curve_to" c_cairo_mesh_pattern_curve_to ::
	Ptr (CairoPatternT s) ->
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoMeshPatternSetControlPointMaybe :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> Maybe Point -> m ()
cairoMeshPatternSetControlPointMaybe (CairoPatternMeshT fpt) i = \case
	Nothing -> pure ()
	Just (Point x y) -> unsafeIOToPrim
		$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_set_control_point ppt i x y

foreign import ccall "cairo_mesh_pattern_set_control_point" c_cairo_mesh_pattern_set_control_point ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> IO ()

cairoMeshPatternSetCornerColor :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> Color -> m ()
cairoMeshPatternSetCornerColor (CairoPatternMeshT fpt) i c =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> case c of
		ColorRgb (RgbDouble r g b) -> c_cairo_mesh_pattern_set_corner_color_rgb ppt i r g b
		ColorRgba (RgbaDouble r g b a) -> c_cairo_mesh_pattern_set_corner_color_rgba ppt i r g b a

foreign import ccall "cairo_mesh_pattern_set_corner_color_rgb" c_cairo_mesh_pattern_set_corner_color_rgb ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall "cairo_mesh_pattern_set_corner_color_rgba" c_cairo_mesh_pattern_set_corner_color_rgba ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoMeshPatternGetPatchCount :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> m CUInt
cairoMeshPatternGetPatchCount (CairoPatternMeshT fpt) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> alloca \cnt -> do
		cairoStatusToThrowError =<< c_cairo_mesh_pattern_get_patch_count ppt cnt
		peek cnt

foreign import ccall "cairo_mesh_pattern_get_patch_count" c_cairo_mesh_pattern_get_patch_count ::
	Ptr (CairoPatternT s) -> Ptr CUInt -> IO #{type cairo_status_t}

cairoMeshPatternGetPath :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> m CairoPatchPathT
cairoMeshPatternGetPath (CairoPatternMeshT fpt) i =
	unsafeIOToPrim $ withForeignPtr fpt \ppt ->
		mkCairoPatchPathT =<< c_cairo_mesh_pattern_get_path ppt i

foreign import ccall "cairo_mesh_pattern_get_path" c_cairo_mesh_pattern_get_path ::
	Ptr (CairoPatternT s) -> CUInt -> IO (Ptr CairoPathT)

cairoMeshPatternGetControlPoint :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> CUInt -> m Point
cairoMeshPatternGetControlPoint (CairoPatternMeshT fpt) i j =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> alloca \x -> alloca \y -> do
		cairoStatusToThrowError =<< c_cairo_mesh_pattern_get_control_point ppt i j x y
		Point <$> peek x <*> peek y

foreign import ccall "cairo_mesh_pattern_get_control_point" c_cairo_mesh_pattern_get_control_point ::
	Ptr (CairoPatternT s) -> CUInt -> CUInt -> Ptr CDouble -> Ptr CDouble -> IO #{type cairo_status_t}

cairoMeshPatternGetCornerColorRgba :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> CUInt -> m (Rgba CDouble)
cairoMeshPatternGetCornerColorRgba (CairoPatternMeshT fpt) i j =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> alloca \r -> alloca \g -> alloca \b -> alloca \a -> do
		cairoStatusToThrowError =<< c_cairo_mesh_pattern_get_corner_color_rgba ppt i j r g b a
		fromJust <$> (rgbaDouble <$> peek r <*> peek g <*> peek b <*> peek a)

foreign import ccall "cairo_mesh_pattern_get_corner_color_rgba" c_cairo_mesh_pattern_get_corner_color_rgba ::
	Ptr (CairoPatternT s) -> CUInt -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO #{type cairo_status_t}

cairoMeshPatternGetPatch1 :: PrimBase m => CairoPatternMeshT (PrimState m) -> CUInt ->
	m (CairoPatchPathT, (Rgba CDouble, Rgba CDouble, Rgba CDouble, Rgba CDouble), (Point, Point, Point, Point))
cairoMeshPatternGetPatch1 pt i = do
	pth <- unsafeInterleave $ cairoMeshPatternGetPath pt i
	(unsafeInterleave . cairoMeshPatternGetCornerColorRgba pt i) `mapM` [0, 1, 2, 3] >>= \case
		[c0, c1, c2, c3] ->
			(unsafeInterleave . cairoMeshPatternGetControlPoint pt i) `mapMLazy` [0, 1, 2, 3] >>= \case
				[p0, p1, p2, p3] -> pure (pth, (c0, c1, c2, c3), (p0, p1, p2, p3))
				_ -> error "never occur"
		_ -> error "never occur"

forLazy :: PrimBase m => [a] -> (a -> m b) -> m [b]
forLazy [] _ = pure []
forLazy (x : xs) f = unsafeInterleave $ (:) <$> f x <*> forLazy xs f

mapMLazy :: PrimBase m => (a -> m b) -> [a] -> m [b]
mapMLazy = flip forLazy

cairoMeshPatternGetPatchList :: PrimBase m => CairoPatternMeshT (PrimState m) ->
	m [(CairoPatchPathT, (Rgba CDouble, Rgba CDouble, Rgba CDouble, Rgba CDouble), (Point, Point, Point, Point))]
cairoMeshPatternGetPatchList pt = do
	n <- cairoMeshPatternGetPatchCount pt
	forLazy [0 .. n - 1] $ unsafeInterleave . cairoMeshPatternGetPatch1 pt
