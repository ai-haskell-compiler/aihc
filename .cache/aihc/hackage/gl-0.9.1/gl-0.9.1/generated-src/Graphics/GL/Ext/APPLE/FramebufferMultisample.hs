-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.FramebufferMultisample (
  -- * Extension Support
    gl_APPLE_framebuffer_multisample

  -- * GL_APPLE_framebuffer_multisample
  , glRenderbufferStorageMultisampleAPPLE
  , glResolveMultisampleFramebufferAPPLE
  , pattern GL_DRAW_FRAMEBUFFER_APPLE
  , pattern GL_DRAW_FRAMEBUFFER_BINDING_APPLE
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_APPLE
  , pattern GL_MAX_SAMPLES_APPLE
  , pattern GL_READ_FRAMEBUFFER_APPLE
  , pattern GL_READ_FRAMEBUFFER_BINDING_APPLE
  , pattern GL_RENDERBUFFER_SAMPLES_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_framebuffer_multisample.txt GL_APPLE_framebuffer_multisample> extension is available.

gl_APPLE_framebuffer_multisample :: Bool
gl_APPLE_framebuffer_multisample = member "GL_APPLE_framebuffer_multisample" extensions
{-# NOINLINE gl_APPLE_framebuffer_multisample #-}

-- | Usage: @'glRenderbufferStorageMultisampleAPPLE' target samples internalformat width height@


glRenderbufferStorageMultisampleAPPLE :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
glRenderbufferStorageMultisampleAPPLE = ffienumsizeienumsizeisizeiIOV glRenderbufferStorageMultisampleAPPLEFunPtr

glRenderbufferStorageMultisampleAPPLEFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
glRenderbufferStorageMultisampleAPPLEFunPtr = unsafePerformIO (getProcAddress "glRenderbufferStorageMultisampleAPPLE")

{-# NOINLINE glRenderbufferStorageMultisampleAPPLEFunPtr #-}

-- | Usage: @'glResolveMultisampleFramebufferAPPLE'@


glResolveMultisampleFramebufferAPPLE :: MonadIO m => m ()
glResolveMultisampleFramebufferAPPLE = ffiIOV glResolveMultisampleFramebufferAPPLEFunPtr

glResolveMultisampleFramebufferAPPLEFunPtr :: FunPtr (IO ())
glResolveMultisampleFramebufferAPPLEFunPtr = unsafePerformIO (getProcAddress "glResolveMultisampleFramebufferAPPLE")

{-# NOINLINE glResolveMultisampleFramebufferAPPLEFunPtr #-}

pattern GL_DRAW_FRAMEBUFFER_APPLE  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_APPLE = 0x8CA9

pattern GL_DRAW_FRAMEBUFFER_BINDING_APPLE  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_BINDING_APPLE = 0x8CA6

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_APPLE  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_APPLE = 0x8D56

pattern GL_MAX_SAMPLES_APPLE  :: (Eq a, Num a) => a

pattern GL_MAX_SAMPLES_APPLE = 0x8D57

pattern GL_READ_FRAMEBUFFER_APPLE  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_APPLE = 0x8CA8

pattern GL_READ_FRAMEBUFFER_BINDING_APPLE  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_BINDING_APPLE = 0x8CAA

pattern GL_RENDERBUFFER_SAMPLES_APPLE  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_SAMPLES_APPLE = 0x8CAB