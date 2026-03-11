-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FramebufferMixedSamples (
  -- * Extension Support
    gl_NV_framebuffer_mixed_samples

  -- * GL_NV_framebuffer_mixed_samples
  , glCoverageModulationNV
  , glCoverageModulationTableNV
  , glGetCoverageModulationTableNV
  , glRasterSamplesEXT
  , pattern GL_COLOR_SAMPLES_NV
  , pattern GL_COVERAGE_MODULATION_NV
  , pattern GL_COVERAGE_MODULATION_TABLE_NV
  , pattern GL_COVERAGE_MODULATION_TABLE_SIZE_NV
  , pattern GL_DEPTH_SAMPLES_NV
  , pattern GL_EFFECTIVE_RASTER_SAMPLES_EXT
  , pattern GL_MAX_RASTER_SAMPLES_EXT
  , pattern GL_MIXED_DEPTH_SAMPLES_SUPPORTED_NV
  , pattern GL_MIXED_STENCIL_SAMPLES_SUPPORTED_NV
  , pattern GL_MULTISAMPLE_RASTERIZATION_ALLOWED_EXT
  , pattern GL_RASTER_FIXED_SAMPLE_LOCATIONS_EXT
  , pattern GL_RASTER_MULTISAMPLE_EXT
  , pattern GL_RASTER_SAMPLES_EXT
  , pattern GL_STENCIL_SAMPLES_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/framebuffer_mixed_samples.txt GL_NV_framebuffer_mixed_samples> extension is available.

gl_NV_framebuffer_mixed_samples :: Bool
gl_NV_framebuffer_mixed_samples = member "GL_NV_framebuffer_mixed_samples" extensions
{-# NOINLINE gl_NV_framebuffer_mixed_samples #-}

-- | Usage: @'glCoverageModulationNV' components@


glCoverageModulationNV :: MonadIO m => GLenum -> m ()
glCoverageModulationNV = ffienumIOV glCoverageModulationNVFunPtr

glCoverageModulationNVFunPtr :: FunPtr (GLenum -> IO ())
glCoverageModulationNVFunPtr = unsafePerformIO (getProcAddress "glCoverageModulationNV")

{-# NOINLINE glCoverageModulationNVFunPtr #-}

-- | Usage: @'glCoverageModulationTableNV' n v@


glCoverageModulationTableNV :: MonadIO m => GLsizei -> Ptr GLfloat -> m ()
glCoverageModulationTableNV = ffisizeiPtrfloatIOV glCoverageModulationTableNVFunPtr

glCoverageModulationTableNVFunPtr :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
glCoverageModulationTableNVFunPtr = unsafePerformIO (getProcAddress "glCoverageModulationTableNV")

{-# NOINLINE glCoverageModulationTableNVFunPtr #-}

-- | Usage: @'glGetCoverageModulationTableNV' bufsize v@


glGetCoverageModulationTableNV :: MonadIO m => GLsizei -> Ptr GLfloat -> m ()
glGetCoverageModulationTableNV = ffisizeiPtrfloatIOV glGetCoverageModulationTableNVFunPtr

glGetCoverageModulationTableNVFunPtr :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
glGetCoverageModulationTableNVFunPtr = unsafePerformIO (getProcAddress "glGetCoverageModulationTableNV")

{-# NOINLINE glGetCoverageModulationTableNVFunPtr #-}

pattern GL_COVERAGE_MODULATION_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_MODULATION_NV = 0x9332

pattern GL_COVERAGE_MODULATION_TABLE_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_MODULATION_TABLE_NV = 0x9331

pattern GL_COVERAGE_MODULATION_TABLE_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_MODULATION_TABLE_SIZE_NV = 0x9333

pattern GL_DEPTH_SAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_SAMPLES_NV = 0x932D

pattern GL_MIXED_DEPTH_SAMPLES_SUPPORTED_NV  :: (Eq a, Num a) => a

pattern GL_MIXED_DEPTH_SAMPLES_SUPPORTED_NV = 0x932F

pattern GL_MIXED_STENCIL_SAMPLES_SUPPORTED_NV  :: (Eq a, Num a) => a

pattern GL_MIXED_STENCIL_SAMPLES_SUPPORTED_NV = 0x9330

pattern GL_STENCIL_SAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_STENCIL_SAMPLES_NV = 0x932E