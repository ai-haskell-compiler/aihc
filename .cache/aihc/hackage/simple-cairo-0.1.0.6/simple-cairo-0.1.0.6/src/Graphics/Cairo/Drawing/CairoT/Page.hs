{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Page where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive
import Data.CairoContext

cairoCopyPage :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoCopyPage (CairoT fcr) =
	unsafeIOToPrim $ withForeignPtr fcr c_cairo_copy_page

foreign import ccall "cairo_copy_page" c_cairo_copy_page ::
	Ptr (CairoT r s) -> IO ()

cairoShowPage :: PrimMonad m => CairoT r (PrimState m) -> m ()
cairoShowPage (CairoT fcr) =
	unsafeIOToPrim $ withForeignPtr fcr c_cairo_show_page

foreign import ccall "cairo_show_page" c_cairo_show_page ::
	Ptr (CairoT r s) -> IO ()
