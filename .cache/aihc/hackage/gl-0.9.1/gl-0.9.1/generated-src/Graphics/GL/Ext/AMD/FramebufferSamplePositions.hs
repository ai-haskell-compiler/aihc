-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.FramebufferSamplePositions (
  -- * Extension Support
    gl_AMD_framebuffer_sample_positions

  -- * GL_AMD_framebuffer_sample_positions
  , glFramebufferSamplePositionsfvAMD
  , glGetFramebufferParameterfvAMD
  , glGetNamedFramebufferParameterfvAMD
  , glNamedFramebufferSamplePositionsfvAMD
  , pattern GL_ALL_PIXELS_AMD
  , pattern GL_PIXELS_PER_SAMPLE_PATTERN_X_AMD
  , pattern GL_PIXELS_PER_SAMPLE_PATTERN_Y_AMD
  , pattern GL_SUBSAMPLE_DISTANCE_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/AMD/AMD_framebuffer_sample_positions.txt GL_AMD_framebuffer_sample_positions> extension is available.

gl_AMD_framebuffer_sample_positions :: Bool
gl_AMD_framebuffer_sample_positions = member "GL_AMD_framebuffer_sample_positions" extensions
{-# NOINLINE gl_AMD_framebuffer_sample_positions #-}

-- | Usage: @'glFramebufferSamplePositionsfvAMD' target numsamples pixelindex values@


glFramebufferSamplePositionsfvAMD :: MonadIO m => GLenum -> GLuint -> GLuint -> Ptr GLfloat -> m ()
glFramebufferSamplePositionsfvAMD = ffienumuintuintPtrfloatIOV glFramebufferSamplePositionsfvAMDFunPtr

glFramebufferSamplePositionsfvAMDFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLfloat -> IO ())
glFramebufferSamplePositionsfvAMDFunPtr = unsafePerformIO (getProcAddress "glFramebufferSamplePositionsfvAMD")

{-# NOINLINE glFramebufferSamplePositionsfvAMDFunPtr #-}

-- | Usage: @'glGetFramebufferParameterfvAMD' target pname numsamples pixelindex size values@


glGetFramebufferParameterfvAMD :: MonadIO m => GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glGetFramebufferParameterfvAMD = ffienumenumuintuintsizeiPtrfloatIOV glGetFramebufferParameterfvAMDFunPtr

glGetFramebufferParameterfvAMDFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glGetFramebufferParameterfvAMDFunPtr = unsafePerformIO (getProcAddress "glGetFramebufferParameterfvAMD")

{-# NOINLINE glGetFramebufferParameterfvAMDFunPtr #-}

-- | Usage: @'glGetNamedFramebufferParameterfvAMD' framebuffer pname numsamples pixelindex size values@


glGetNamedFramebufferParameterfvAMD :: MonadIO m => GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glGetNamedFramebufferParameterfvAMD = ffienumenumuintuintsizeiPtrfloatIOV glGetNamedFramebufferParameterfvAMDFunPtr

glGetNamedFramebufferParameterfvAMDFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glGetNamedFramebufferParameterfvAMDFunPtr = unsafePerformIO (getProcAddress "glGetNamedFramebufferParameterfvAMD")

{-# NOINLINE glGetNamedFramebufferParameterfvAMDFunPtr #-}

-- | Usage: @'glNamedFramebufferSamplePositionsfvAMD' framebuffer numsamples pixelindex values@


glNamedFramebufferSamplePositionsfvAMD :: MonadIO m => GLuint -> GLuint -> GLuint -> Ptr GLfloat -> m ()
glNamedFramebufferSamplePositionsfvAMD = ffiuintuintuintPtrfloatIOV glNamedFramebufferSamplePositionsfvAMDFunPtr

glNamedFramebufferSamplePositionsfvAMDFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLfloat -> IO ())
glNamedFramebufferSamplePositionsfvAMDFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferSamplePositionsfvAMD")

{-# NOINLINE glNamedFramebufferSamplePositionsfvAMDFunPtr #-}

pattern GL_ALL_PIXELS_AMD  :: (Eq a, Num a) => a

pattern GL_ALL_PIXELS_AMD = 0xFFFFFFFF

pattern GL_PIXELS_PER_SAMPLE_PATTERN_X_AMD  :: (Eq a, Num a) => a

pattern GL_PIXELS_PER_SAMPLE_PATTERN_X_AMD = 0x91AE

pattern GL_PIXELS_PER_SAMPLE_PATTERN_Y_AMD  :: (Eq a, Num a) => a

pattern GL_PIXELS_PER_SAMPLE_PATTERN_Y_AMD = 0x91AF