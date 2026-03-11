-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.EGLImage (
  -- * Extension Support
    gl_OES_EGL_image

  -- * GL_OES_EGL_image
  , glEGLImageTargetRenderbufferStorageOES
  , glEGLImageTargetTexture2DOES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_EGL_image.txt GL_OES_EGL_image> extension is available.

gl_OES_EGL_image :: Bool
gl_OES_EGL_image = member "GL_OES_EGL_image" extensions
{-# NOINLINE gl_OES_EGL_image #-}

-- | Usage: @'glEGLImageTargetRenderbufferStorageOES' target image@


glEGLImageTargetRenderbufferStorageOES :: MonadIO m => GLenum -> GLeglImageOES -> m ()
glEGLImageTargetRenderbufferStorageOES = ffienumeglImageOESIOV glEGLImageTargetRenderbufferStorageOESFunPtr

glEGLImageTargetRenderbufferStorageOESFunPtr :: FunPtr (GLenum -> GLeglImageOES -> IO ())
glEGLImageTargetRenderbufferStorageOESFunPtr = unsafePerformIO (getProcAddress "glEGLImageTargetRenderbufferStorageOES")

{-# NOINLINE glEGLImageTargetRenderbufferStorageOESFunPtr #-}

-- | Usage: @'glEGLImageTargetTexture2DOES' target image@


glEGLImageTargetTexture2DOES :: MonadIO m => GLenum -> GLeglImageOES -> m ()
glEGLImageTargetTexture2DOES = ffienumeglImageOESIOV glEGLImageTargetTexture2DOESFunPtr

glEGLImageTargetTexture2DOESFunPtr :: FunPtr (GLenum -> GLeglImageOES -> IO ())
glEGLImageTargetTexture2DOESFunPtr = unsafePerformIO (getProcAddress "glEGLImageTargetTexture2DOES")

{-# NOINLINE glEGLImageTargetTexture2DOESFunPtr #-}