-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FramebufferMultisample (
  -- * Extension Support
    gl_NV_framebuffer_multisample

  -- * GL_NV_framebuffer_multisample
  , glRenderbufferStorageMultisampleNV
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_NV
  , pattern GL_MAX_SAMPLES_NV
  , pattern GL_RENDERBUFFER_SAMPLES_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_framebuffer_multisample.txt GL_NV_framebuffer_multisample> extension is available.

gl_NV_framebuffer_multisample :: Bool
gl_NV_framebuffer_multisample = member "GL_NV_framebuffer_multisample" extensions
{-# NOINLINE gl_NV_framebuffer_multisample #-}

-- | Usage: @'glRenderbufferStorageMultisampleNV' target samples internalformat width height@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glRenderbufferStorageMultisample'.


glRenderbufferStorageMultisampleNV :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
glRenderbufferStorageMultisampleNV = ffienumsizeienumsizeisizeiIOV glRenderbufferStorageMultisampleNVFunPtr

glRenderbufferStorageMultisampleNVFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
glRenderbufferStorageMultisampleNVFunPtr = unsafePerformIO (getProcAddress "glRenderbufferStorageMultisampleNV")

{-# NOINLINE glRenderbufferStorageMultisampleNVFunPtr #-}

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_NV  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_NV = 0x8D56

pattern GL_MAX_SAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_MAX_SAMPLES_NV = 0x8D57

pattern GL_RENDERBUFFER_SAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_SAMPLES_NV = 0x8CAB