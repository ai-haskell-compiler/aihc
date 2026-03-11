{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths.CopyAppend where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive
import Data.CairoContext
import Graphics.Cairo.Drawing.Paths.CairoPathT

cairoCopyPath :: PrimMonad m => CairoT r (PrimState m) -> m CairoPathT
cairoCopyPath (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> mkCairoPathT =<< c_cairo_copy_path pcr

foreign import ccall "cairo_copy_path" c_cairo_copy_path ::
	Ptr (CairoT r s) -> IO (Ptr CairoPathT)

cairoCopyPathFlat :: PrimMonad m => CairoT r (PrimState m) -> m CairoPathT
cairoCopyPathFlat (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> mkCairoPathT =<< c_cairo_copy_path_flat pcr

foreign import ccall "cairo_copy_path_flat" c_cairo_copy_path_flat ::
	Ptr (CairoT r s) -> IO (Ptr CairoPathT)

cairoAppendPath :: PrimMonad m =>
	CairoT r (PrimState m) -> CairoPathT -> m ()
cairoAppendPath (CairoT fcr) pth = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withCairoPathT pth \ppth ->
		c_cairo_append_path pcr ppth

foreign import ccall "cairo_append_path" c_cairo_append_path ::
	Ptr (CairoT r s) -> Ptr CairoPathT -> IO ()
