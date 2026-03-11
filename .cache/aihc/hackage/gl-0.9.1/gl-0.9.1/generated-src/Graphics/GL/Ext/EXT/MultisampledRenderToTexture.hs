-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.MultisampledRenderToTexture (
  -- * Extension Support
    gl_EXT_multisampled_render_to_texture

  -- * GL_EXT_multisampled_render_to_texture
  , glFramebufferTexture2DMultisampleEXT
  , glRenderbufferStorageMultisampleEXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_SAMPLES_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT
  , pattern GL_MAX_SAMPLES_EXT
  , pattern GL_RENDERBUFFER_SAMPLES_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_multisampled_render_to_texture.txt GL_EXT_multisampled_render_to_texture> extension is available.

gl_EXT_multisampled_render_to_texture :: Bool
gl_EXT_multisampled_render_to_texture = member "GL_EXT_multisampled_render_to_texture" extensions
{-# NOINLINE gl_EXT_multisampled_render_to_texture #-}

-- | Usage: @'glFramebufferTexture2DMultisampleEXT' target attachment textarget texture level samples@


glFramebufferTexture2DMultisampleEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> m ()
glFramebufferTexture2DMultisampleEXT = ffienumenumenumuintintsizeiIOV glFramebufferTexture2DMultisampleEXTFunPtr

glFramebufferTexture2DMultisampleEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ())
glFramebufferTexture2DMultisampleEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture2DMultisampleEXT")

{-# NOINLINE glFramebufferTexture2DMultisampleEXTFunPtr #-}

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_SAMPLES_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_SAMPLES_EXT = 0x8D6C