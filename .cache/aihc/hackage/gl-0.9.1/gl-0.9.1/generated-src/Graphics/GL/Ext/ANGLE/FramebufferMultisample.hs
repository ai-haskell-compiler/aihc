-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.FramebufferMultisample (
  -- * Extension Support
    gl_ANGLE_framebuffer_multisample

  -- * GL_ANGLE_framebuffer_multisample
  , glRenderbufferStorageMultisampleANGLE
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_ANGLE
  , pattern GL_MAX_SAMPLES_ANGLE
  , pattern GL_RENDERBUFFER_SAMPLES_ANGLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_framebuffer_multisample.txt GL_ANGLE_framebuffer_multisample> extension is available.

gl_ANGLE_framebuffer_multisample :: Bool
gl_ANGLE_framebuffer_multisample = member "GL_ANGLE_framebuffer_multisample" extensions
{-# NOINLINE gl_ANGLE_framebuffer_multisample #-}

-- | Usage: @'glRenderbufferStorageMultisampleANGLE' target samples internalformat width height@


glRenderbufferStorageMultisampleANGLE :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
glRenderbufferStorageMultisampleANGLE = ffienumsizeienumsizeisizeiIOV glRenderbufferStorageMultisampleANGLEFunPtr

glRenderbufferStorageMultisampleANGLEFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
glRenderbufferStorageMultisampleANGLEFunPtr = unsafePerformIO (getProcAddress "glRenderbufferStorageMultisampleANGLE")

{-# NOINLINE glRenderbufferStorageMultisampleANGLEFunPtr #-}

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_ANGLE  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_ANGLE = 0x8D56

pattern GL_MAX_SAMPLES_ANGLE  :: (Eq a, Num a) => a

pattern GL_MAX_SAMPLES_ANGLE = 0x8D57

pattern GL_RENDERBUFFER_SAMPLES_ANGLE  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_SAMPLES_ANGLE = 0x8CAB