-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OVR.Multiview (
  -- * Extension Support
    gl_OVR_multiview

  -- * GL_OVR_multiview
  , glFramebufferTextureMultiviewOVR
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_BASE_VIEW_INDEX_OVR
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_NUM_VIEWS_OVR
  , pattern GL_FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR
  , pattern GL_MAX_VIEWS_OVR
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OVR/multiview.txt GL_OVR_multiview> extension is available.

gl_OVR_multiview :: Bool
gl_OVR_multiview = member "GL_OVR_multiview" extensions
{-# NOINLINE gl_OVR_multiview #-}

-- | Usage: @'glFramebufferTextureMultiviewOVR' target attachment texture level baseViewIndex numViews@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.


glFramebufferTextureMultiviewOVR :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> m ()
glFramebufferTextureMultiviewOVR = ffienumenumuintintintsizeiIOV glFramebufferTextureMultiviewOVRFunPtr

glFramebufferTextureMultiviewOVRFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> IO ())
glFramebufferTextureMultiviewOVRFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureMultiviewOVR")

{-# NOINLINE glFramebufferTextureMultiviewOVRFunPtr #-}

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_BASE_VIEW_INDEX_OVR  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_BASE_VIEW_INDEX_OVR = 0x9632

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_NUM_VIEWS_OVR  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_NUM_VIEWS_OVR = 0x9630

pattern GL_FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR = 0x9633

pattern GL_MAX_VIEWS_OVR  :: (Eq a, Num a) => a

pattern GL_MAX_VIEWS_OVR = 0x9631