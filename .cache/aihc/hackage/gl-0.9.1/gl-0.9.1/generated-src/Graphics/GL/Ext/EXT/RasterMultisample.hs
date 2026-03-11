-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.RasterMultisample (
  -- * Extension Support
    gl_EXT_raster_multisample

  -- * GL_EXT_raster_multisample
  , glRasterSamplesEXT
  , pattern GL_EFFECTIVE_RASTER_SAMPLES_EXT
  , pattern GL_MAX_RASTER_SAMPLES_EXT
  , pattern GL_MULTISAMPLE_RASTERIZATION_ALLOWED_EXT
  , pattern GL_RASTER_FIXED_SAMPLE_LOCATIONS_EXT
  , pattern GL_RASTER_MULTISAMPLE_EXT
  , pattern GL_RASTER_SAMPLES_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/raster_multisample.txt GL_EXT_raster_multisample> extension is available.

gl_EXT_raster_multisample :: Bool
gl_EXT_raster_multisample = member "GL_EXT_raster_multisample" extensions
{-# NOINLINE gl_EXT_raster_multisample #-}