-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.MultisampledRenderToTexture (
  -- * Extension Support
    gl_IMG_multisampled_render_to_texture

  -- * GL_IMG_multisampled_render_to_texture
  , glFramebufferTexture2DMultisampleIMG
  , glRenderbufferStorageMultisampleIMG
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_IMG
  , pattern GL_MAX_SAMPLES_IMG
  , pattern GL_RENDERBUFFER_SAMPLES_IMG
  , pattern GL_TEXTURE_SAMPLES_IMG
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_multisampled_render_to_texture.txt GL_IMG_multisampled_render_to_texture> extension is available.

gl_IMG_multisampled_render_to_texture :: Bool
gl_IMG_multisampled_render_to_texture = member "GL_IMG_multisampled_render_to_texture" extensions
{-# NOINLINE gl_IMG_multisampled_render_to_texture #-}

-- | Usage: @'glFramebufferTexture2DMultisampleIMG' target attachment textarget texture level samples@


glFramebufferTexture2DMultisampleIMG :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> m ()
glFramebufferTexture2DMultisampleIMG = ffienumenumenumuintintsizeiIOV glFramebufferTexture2DMultisampleIMGFunPtr

glFramebufferTexture2DMultisampleIMGFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ())
glFramebufferTexture2DMultisampleIMGFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture2DMultisampleIMG")

{-# NOINLINE glFramebufferTexture2DMultisampleIMGFunPtr #-}

-- | Usage: @'glRenderbufferStorageMultisampleIMG' target samples internalformat width height@


glRenderbufferStorageMultisampleIMG :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
glRenderbufferStorageMultisampleIMG = ffienumsizeienumsizeisizeiIOV glRenderbufferStorageMultisampleIMGFunPtr

glRenderbufferStorageMultisampleIMGFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
glRenderbufferStorageMultisampleIMGFunPtr = unsafePerformIO (getProcAddress "glRenderbufferStorageMultisampleIMG")

{-# NOINLINE glRenderbufferStorageMultisampleIMGFunPtr #-}

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_IMG  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_IMG = 0x9134

pattern GL_MAX_SAMPLES_IMG  :: (Eq a, Num a) => a

pattern GL_MAX_SAMPLES_IMG = 0x9135

pattern GL_RENDERBUFFER_SAMPLES_IMG  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_SAMPLES_IMG = 0x9133

pattern GL_TEXTURE_SAMPLES_IMG  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SAMPLES_IMG = 0x9136