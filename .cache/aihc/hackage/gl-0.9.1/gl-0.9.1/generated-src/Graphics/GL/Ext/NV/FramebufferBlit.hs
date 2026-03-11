-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FramebufferBlit (
  -- * Extension Support
    gl_NV_framebuffer_blit

  -- * GL_NV_framebuffer_blit
  , glBlitFramebufferNV
  , pattern GL_DRAW_FRAMEBUFFER_BINDING_NV
  , pattern GL_DRAW_FRAMEBUFFER_NV
  , pattern GL_READ_FRAMEBUFFER_BINDING_NV
  , pattern GL_READ_FRAMEBUFFER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_framebuffer_blit.txt GL_NV_framebuffer_blit> extension is available.

gl_NV_framebuffer_blit :: Bool
gl_NV_framebuffer_blit = member "GL_NV_framebuffer_blit" extensions
{-# NOINLINE gl_NV_framebuffer_blit #-}

-- | Usage: @'glBlitFramebufferNV' srcX0 srcY0 srcX1 srcY1 dstX0 dstY0 dstX1 dstY1 mask filter@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlitFramebuffer'.


glBlitFramebufferNV :: MonadIO m => GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> m ()
glBlitFramebufferNV = ffiintintintintintintintintbitfieldenumIOV glBlitFramebufferNVFunPtr

glBlitFramebufferNVFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
glBlitFramebufferNVFunPtr = unsafePerformIO (getProcAddress "glBlitFramebufferNV")

{-# NOINLINE glBlitFramebufferNVFunPtr #-}

pattern GL_DRAW_FRAMEBUFFER_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_BINDING_NV = 0x8CA6

pattern GL_DRAW_FRAMEBUFFER_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_NV = 0x8CA9

pattern GL_READ_FRAMEBUFFER_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_BINDING_NV = 0x8CAA

pattern GL_READ_FRAMEBUFFER_NV  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_NV = 0x8CA8