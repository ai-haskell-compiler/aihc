-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ConservativeRasterDilate (
  -- * Extension Support
    gl_NV_conservative_raster_dilate

  -- * GL_NV_conservative_raster_dilate
  , glConservativeRasterParameterfNV
  , pattern GL_CONSERVATIVE_RASTER_DILATE_GRANULARITY_NV
  , pattern GL_CONSERVATIVE_RASTER_DILATE_NV
  , pattern GL_CONSERVATIVE_RASTER_DILATE_RANGE_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/conservative_raster_dilate.txt GL_NV_conservative_raster_dilate> extension is available.

gl_NV_conservative_raster_dilate :: Bool
gl_NV_conservative_raster_dilate = member "GL_NV_conservative_raster_dilate" extensions
{-# NOINLINE gl_NV_conservative_raster_dilate #-}

-- | Usage: @'glConservativeRasterParameterfNV' pname value@


glConservativeRasterParameterfNV :: MonadIO m => GLenum -> GLfloat -> m ()
glConservativeRasterParameterfNV = ffienumfloatIOV glConservativeRasterParameterfNVFunPtr

glConservativeRasterParameterfNVFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glConservativeRasterParameterfNVFunPtr = unsafePerformIO (getProcAddress "glConservativeRasterParameterfNV")

{-# NOINLINE glConservativeRasterParameterfNVFunPtr #-}

pattern GL_CONSERVATIVE_RASTER_DILATE_GRANULARITY_NV  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTER_DILATE_GRANULARITY_NV = 0x937B

pattern GL_CONSERVATIVE_RASTER_DILATE_NV  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTER_DILATE_NV = 0x9379

pattern GL_CONSERVATIVE_RASTER_DILATE_RANGE_NV  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTER_DILATE_RANGE_NV = 0x937A