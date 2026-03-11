-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.FlushRaster (
  -- * Extension Support
    gl_SGIX_flush_raster

  -- * GL_SGIX_flush_raster
  , glFlushRasterSGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/flush_raster.txt GL_SGIX_flush_raster> extension is available.

gl_SGIX_flush_raster :: Bool
gl_SGIX_flush_raster = member "GL_SGIX_flush_raster" extensions
{-# NOINLINE gl_SGIX_flush_raster #-}

-- | Usage: @'glFlushRasterSGIX'@


glFlushRasterSGIX :: MonadIO m => m ()
glFlushRasterSGIX = ffiIOV glFlushRasterSGIXFunPtr

glFlushRasterSGIXFunPtr :: FunPtr (IO ())
glFlushRasterSGIXFunPtr = unsafePerformIO (getProcAddress "glFlushRasterSGIX")

{-# NOINLINE glFlushRasterSGIXFunPtr #-}