-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ConservativeRaster (
  -- * Extension Support
    gl_NV_conservative_raster

  -- * GL_NV_conservative_raster
  , glSubpixelPrecisionBiasNV
  , pattern GL_CONSERVATIVE_RASTERIZATION_NV
  , pattern GL_MAX_SUBPIXEL_PRECISION_BIAS_BITS_NV
  , pattern GL_SUBPIXEL_PRECISION_BIAS_X_BITS_NV
  , pattern GL_SUBPIXEL_PRECISION_BIAS_Y_BITS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/conservative_raster.txt GL_NV_conservative_raster> extension is available.

gl_NV_conservative_raster :: Bool
gl_NV_conservative_raster = member "GL_NV_conservative_raster" extensions
{-# NOINLINE gl_NV_conservative_raster #-}

-- | Usage: @'glSubpixelPrecisionBiasNV' xbits ybits@


glSubpixelPrecisionBiasNV :: MonadIO m => GLuint -> GLuint -> m ()
glSubpixelPrecisionBiasNV = ffiuintuintIOV glSubpixelPrecisionBiasNVFunPtr

glSubpixelPrecisionBiasNVFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glSubpixelPrecisionBiasNVFunPtr = unsafePerformIO (getProcAddress "glSubpixelPrecisionBiasNV")

{-# NOINLINE glSubpixelPrecisionBiasNVFunPtr #-}

pattern GL_CONSERVATIVE_RASTERIZATION_NV  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTERIZATION_NV = 0x9346

pattern GL_MAX_SUBPIXEL_PRECISION_BIAS_BITS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_SUBPIXEL_PRECISION_BIAS_BITS_NV = 0x9349

pattern GL_SUBPIXEL_PRECISION_BIAS_X_BITS_NV  :: (Eq a, Num a) => a

pattern GL_SUBPIXEL_PRECISION_BIAS_X_BITS_NV = 0x9347

pattern GL_SUBPIXEL_PRECISION_BIAS_Y_BITS_NV  :: (Eq a, Num a) => a

pattern GL_SUBPIXEL_PRECISION_BIAS_Y_BITS_NV = 0x9348