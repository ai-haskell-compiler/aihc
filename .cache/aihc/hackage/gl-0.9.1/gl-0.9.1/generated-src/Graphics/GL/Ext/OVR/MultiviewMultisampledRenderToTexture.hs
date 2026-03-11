-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OVR.MultiviewMultisampledRenderToTexture (
  -- * Extension Support
    gl_OVR_multiview_multisampled_render_to_texture

  -- * GL_OVR_multiview_multisampled_render_to_texture
  , glFramebufferTextureMultisampleMultiviewOVR
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/OVR/OVR_multiview_multisampled_render_to_texture.txt GL_OVR_multiview_multisampled_render_to_texture> extension is available.

gl_OVR_multiview_multisampled_render_to_texture :: Bool
gl_OVR_multiview_multisampled_render_to_texture = member "GL_OVR_multiview_multisampled_render_to_texture" extensions
{-# NOINLINE gl_OVR_multiview_multisampled_render_to_texture #-}

-- | Usage: @'glFramebufferTextureMultisampleMultiviewOVR' target attachment texture level samples baseViewIndex numViews@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.


glFramebufferTextureMultisampleMultiviewOVR :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> m ()
glFramebufferTextureMultisampleMultiviewOVR = ffienumenumuintintsizeiintsizeiIOV glFramebufferTextureMultisampleMultiviewOVRFunPtr

glFramebufferTextureMultisampleMultiviewOVRFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> IO ())
glFramebufferTextureMultisampleMultiviewOVRFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureMultisampleMultiviewOVR")

{-# NOINLINE glFramebufferTextureMultisampleMultiviewOVRFunPtr #-}