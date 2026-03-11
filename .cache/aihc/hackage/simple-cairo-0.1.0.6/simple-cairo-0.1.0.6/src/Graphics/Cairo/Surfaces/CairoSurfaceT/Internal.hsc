{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceT.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad.Primitive
import Control.Concurrent.STM
import Data.Word
import System.IO.Unsafe

import Graphics.Cairo.Surfaces.CairoSurfaceTypeT

import Graphics.Cairo.Template

#include <cairo.h>

class IsCairoSurfaceT sr where
	toCairoSurfaceT :: sr s ps -> CairoSurfaceT s ps
	cairoSurfaceTFinishChecker :: sr s ps -> STM (TChan ())

	cairoSurfaceTFinishChecker _ = newTChan >>= \c -> c <$ writeTChan c ()

instance IsCairoSurfaceT CairoSurfaceT where toCairoSurfaceT = id

newtype CairoSurfaceT s ps = CairoSurfaceT (ForeignPtr (CairoSurfaceT s ps)) deriving Show

mkCairoSurfaceT :: Ptr (CairoSurfaceT s ps) -> IO (CairoSurfaceT s ps)
mkCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr (CairoSurfaceT s ps) -> IO ()

foreign import ccall "cairo_surface_finish" c_cairo_surface_finish ::
	Ptr (CairoSurfaceT s ps) -> IO ()

cairoSurfaceFlush :: (IsCairoSurfaceT sr, PrimMonad m) => sr s (PrimState m) -> m ()
cairoSurfaceFlush (toCairoSurfaceT -> CairoSurfaceT fsr) = unsafeIOToPrim
	$ withForeignPtr fsr c_cairo_surface_flush

foreign import ccall "cairo_surface_flush" c_cairo_surface_flush ::
	Ptr (CairoSurfaceT s ps) -> IO ()

cairoSurfaceGetType :: IsCairoSurfaceT sr => sr s ps -> CairoSurfaceTypeT
cairoSurfaceGetType (toCairoSurfaceT -> CairoSurfaceT fsr) = unsafePerformIO
	$ CairoSurfaceTypeT <$> withForeignPtr fsr c_cairo_surface_get_type

foreign import ccall "cairo_surface_get_type" c_cairo_surface_get_type ::
	Ptr (CairoSurfaceT s ps) -> IO #{type cairo_surface_type_t}

cairoSurfaceGetContent :: IsCairoSurfaceT sr => sr s ps -> CairoContentT
cairoSurfaceGetContent (toCairoSurfaceT -> CairoSurfaceT fsr) = unsafePerformIO
	$ CairoContentT <$> withForeignPtr fsr c_cairo_surface_get_content

foreign import ccall "cairo_surface_get_content" c_cairo_surface_get_content ::
	Ptr (CairoSurfaceT s ps) -> IO #{type cairo_content_t}

newtype CairoContentT = CairoContentT #{type cairo_content_t} deriving Show

mkMemberGen ''CairoContentT 'CairoContentT "CairoContentColor" #{const CAIRO_CONTENT_COLOR}
mkMemberGen ''CairoContentT 'CairoContentT "CairoContentAlpha" #{const CAIRO_CONTENT_ALPHA}
mkMemberGen ''CairoContentT 'CairoContentT "CairoContentColorAlpha" #{const CAIRO_CONTENT_COLOR_ALPHA}
