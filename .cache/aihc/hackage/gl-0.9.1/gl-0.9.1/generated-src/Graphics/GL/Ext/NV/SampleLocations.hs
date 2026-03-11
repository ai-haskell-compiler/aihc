-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.SampleLocations (
  -- * Extension Support
    gl_NV_sample_locations

  -- * GL_NV_sample_locations
  , glFramebufferSampleLocationsfvNV
  , glNamedFramebufferSampleLocationsfvNV
  , glResolveDepthValuesNV
  , pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_NV
  , pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_NV
  , pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_NV
  , pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_NV
  , pattern GL_SAMPLE_LOCATION_NV
  , pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_NV
  , pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_NV
  , pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/sample_locations.txt GL_NV_sample_locations> extension is available.

gl_NV_sample_locations :: Bool
gl_NV_sample_locations = member "GL_NV_sample_locations" extensions
{-# NOINLINE gl_NV_sample_locations #-}

-- | Usage: @'glFramebufferSampleLocationsfvNV' target start count v@


glFramebufferSampleLocationsfvNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glFramebufferSampleLocationsfvNV = ffienumuintsizeiPtrfloatIOV glFramebufferSampleLocationsfvNVFunPtr

glFramebufferSampleLocationsfvNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glFramebufferSampleLocationsfvNVFunPtr = unsafePerformIO (getProcAddress "glFramebufferSampleLocationsfvNV")

{-# NOINLINE glFramebufferSampleLocationsfvNVFunPtr #-}

-- | Usage: @'glNamedFramebufferSampleLocationsfvNV' framebuffer start count v@


glNamedFramebufferSampleLocationsfvNV :: MonadIO m => GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glNamedFramebufferSampleLocationsfvNV = ffiuintuintsizeiPtrfloatIOV glNamedFramebufferSampleLocationsfvNVFunPtr

glNamedFramebufferSampleLocationsfvNVFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glNamedFramebufferSampleLocationsfvNVFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferSampleLocationsfvNV")

{-# NOINLINE glNamedFramebufferSampleLocationsfvNVFunPtr #-}

-- | Usage: @'glResolveDepthValuesNV'@


glResolveDepthValuesNV :: MonadIO m => m ()
glResolveDepthValuesNV = ffiIOV glResolveDepthValuesNVFunPtr

glResolveDepthValuesNVFunPtr :: FunPtr (IO ())
glResolveDepthValuesNVFunPtr = unsafePerformIO (getProcAddress "glResolveDepthValuesNV")

{-# NOINLINE glResolveDepthValuesNVFunPtr #-}

pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_NV  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_NV = 0x9342

pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_NV  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_NV = 0x9343

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_NV = 0x9341

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_NV = 0x9340

pattern GL_SAMPLE_LOCATION_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_NV = 0x8E50

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_NV = 0x933F

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_NV = 0x933E

pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_NV = 0x933D