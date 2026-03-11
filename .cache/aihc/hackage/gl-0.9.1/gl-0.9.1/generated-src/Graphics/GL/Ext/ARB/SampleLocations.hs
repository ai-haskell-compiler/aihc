-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SampleLocations (
  -- * Extension Support
    gl_ARB_sample_locations

  -- * GL_ARB_sample_locations
  , glEvaluateDepthValuesARB
  , glFramebufferSampleLocationsfvARB
  , glNamedFramebufferSampleLocationsfvARB
  , pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_ARB
  , pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_ARB
  , pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_ARB
  , pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_ARB
  , pattern GL_SAMPLE_LOCATION_ARB
  , pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_ARB
  , pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_ARB
  , pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sample_locations.txt GL_ARB_sample_locations> extension is available.

gl_ARB_sample_locations :: Bool
gl_ARB_sample_locations = member "GL_ARB_sample_locations" extensions
{-# NOINLINE gl_ARB_sample_locations #-}

-- | Usage: @'glEvaluateDepthValuesARB'@


glEvaluateDepthValuesARB :: MonadIO m => m ()
glEvaluateDepthValuesARB = ffiIOV glEvaluateDepthValuesARBFunPtr

glEvaluateDepthValuesARBFunPtr :: FunPtr (IO ())
glEvaluateDepthValuesARBFunPtr = unsafePerformIO (getProcAddress "glEvaluateDepthValuesARB")

{-# NOINLINE glEvaluateDepthValuesARBFunPtr #-}

-- | Usage: @'glFramebufferSampleLocationsfvARB' target start count v@


glFramebufferSampleLocationsfvARB :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glFramebufferSampleLocationsfvARB = ffienumuintsizeiPtrfloatIOV glFramebufferSampleLocationsfvARBFunPtr

glFramebufferSampleLocationsfvARBFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glFramebufferSampleLocationsfvARBFunPtr = unsafePerformIO (getProcAddress "glFramebufferSampleLocationsfvARB")

{-# NOINLINE glFramebufferSampleLocationsfvARBFunPtr #-}

-- | Usage: @'glNamedFramebufferSampleLocationsfvARB' framebuffer start count v@


glNamedFramebufferSampleLocationsfvARB :: MonadIO m => GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glNamedFramebufferSampleLocationsfvARB = ffiuintuintsizeiPtrfloatIOV glNamedFramebufferSampleLocationsfvARBFunPtr

glNamedFramebufferSampleLocationsfvARBFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glNamedFramebufferSampleLocationsfvARBFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferSampleLocationsfvARB")

{-# NOINLINE glNamedFramebufferSampleLocationsfvARBFunPtr #-}

pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_ARB  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_ARB = 0x9342

pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_ARB  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_ARB = 0x9343

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_ARB  :: (Eq a, Num a) => a

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_ARB = 0x9341

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_ARB = 0x9340

pattern GL_SAMPLE_LOCATION_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_ARB = 0x8E50

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_ARB = 0x933F

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_ARB = 0x933E

pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_ARB = 0x933D