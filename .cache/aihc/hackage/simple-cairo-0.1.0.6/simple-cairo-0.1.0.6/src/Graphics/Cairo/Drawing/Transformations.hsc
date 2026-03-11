{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Transformations where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal
import Control.Monad
import Control.Monad.Primitive

import Data.CairoContext

import Graphics.Cairo.Utilities.CairoMatrixT.Internal

#include <cairo.h>

cairoTranslate :: PrimMonad m =>
	CairoT r (PrimState m) -> CDouble -> CDouble -> m ()
cairoTranslate (CairoT fcr) tx ty =
	unsafeIOToPrim $ withForeignPtr fcr \cr -> c_cairo_translate cr tx ty

foreign import ccall "cairo_translate" c_cairo_translate ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO ()

cairoScale :: PrimMonad m =>
	CairoT r (PrimState m) -> CDouble -> CDouble -> m ()
cairoScale (CairoT fcr) sx sy =
	unsafeIOToPrim $ withForeignPtr fcr \cr -> c_cairo_scale cr sx sy

foreign import ccall "cairo_scale" c_cairo_scale ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> IO ()

cairoRotate :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> m ()
cairoRotate (CairoT fcr) a =
	unsafeIOToPrim $ withForeignPtr fcr \cr -> c_cairo_rotate cr a

foreign import ccall "cairo_rotate" c_cairo_rotate ::
	Ptr (CairoT r s) -> CDouble -> IO ()

cairoTransform :: (PrimMonad m, IsCairoMatrixT mtx) =>
	CairoT r (PrimState m) -> mtx (PrimState m) -> m ()
cairoTransform (CairoT fcr) (toCairoMatrixT -> CairoMatrixT fmtx) =
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> withForeignPtr fmtx \pmtx ->
		c_cairo_transform pcr pmtx

foreign import ccall "cairo_transform" c_cairo_transform ::
	Ptr (CairoT r s) -> Ptr (CairoMatrixT s) -> IO ()

cairoSetMatrix :: (PrimMonad m, IsCairoMatrixT mtx) =>
	CairoT r (PrimState m) -> mtx (PrimState m) -> m ()
cairoSetMatrix (CairoT fcr) (toCairoMatrixT -> CairoMatrixT fmtx) =
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> withForeignPtr fmtx \pmtx ->
		c_cairo_set_matrix pcr pmtx

cairoGetMatrix :: PrimMonad m => CairoT r (PrimState m) -> m (CairoMatrixT (PrimState m))
cairoGetMatrix (CairoT fcr) = unsafeIOToPrim
	$ CairoMatrixT <$> withForeignPtr fcr \pcr -> do
		p <- mallocBytes #{size cairo_matrix_t}
		c_cairo_get_matrix pcr p
		newForeignPtr p (free p)

foreign import ccall "cairo_set_matrix" c_cairo_set_matrix ::
	Ptr (CairoT r s) -> Ptr (CairoMatrixT s) -> IO ()

foreign import ccall "cairo_get_matrix" c_cairo_get_matrix ::
	Ptr (CairoT r s) -> Ptr (CairoMatrixT s) -> IO ()

cairoIdentityMatrix :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoIdentityMatrix (CairoT fcr) =
	unsafeIOToPrim $ withForeignPtr fcr c_cairo_identity_matrix

foreign import ccall "cairo_identity_matrix" c_cairo_identity_matrix :: Ptr (CairoT r s) -> IO ()

cairoUserToDevice :: PrimMonad m =>
	CairoT r (PrimState m) -> CDouble -> CDouble -> m (CDouble, CDouble)
cairoUserToDevice (CairoT fcr) x y =
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> alloca \px -> alloca \py -> do
		zipWithM_ poke [px, py] [x, y]
		c_cairo_user_to_device pcr px py
		(,) <$> peek px <*> peek py

foreign import ccall "cairo_user_to_device" c_cairo_user_to_device ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoUserToDeviceDistance :: PrimMonad m =>
	CairoT r (PrimState m) -> CDouble -> CDouble -> m (CDouble, CDouble)
cairoUserToDeviceDistance (CairoT fcr) dx dy =
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> alloca \px -> alloca \py -> do
		zipWithM_ poke [px, py] [dx, dy]
		c_cairo_user_to_device_distance pcr px py
		(,) <$> peek px <*> peek py

foreign import ccall "cairo_user_to_device_distance" c_cairo_user_to_device_distance ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoDeviceToUser :: PrimMonad m =>
	(CairoT r (PrimState m)) -> CDouble -> CDouble -> m (CDouble, CDouble)
cairoDeviceToUser (CairoT fcr) x y =
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> alloca \px -> alloca \py -> do
		zipWithM_ poke [px, py] [x, y]
		c_cairo_device_to_user pcr px py
		(,) <$> peek px <*> peek py

foreign import ccall "cairo_device_to_user" c_cairo_device_to_user ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoDeviceToUserDistance :: PrimMonad m =>
	(CairoT r (PrimState m)) -> CDouble -> CDouble -> m (CDouble, CDouble)
cairoDeviceToUserDistance (CairoT fcr) dx dy =
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> alloca \px -> alloca \py -> do
		zipWithM_ poke [px, py] [dx, dy]
		c_cairo_device_to_user_distance pcr px py
		(,) <$> peek px <*> peek py

foreign import ccall "cairo_device_to_user_distance" c_cairo_device_to_user_distance ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> IO ()
