-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.FramebufferBlit (
  -- * Extension Support
    gl_ANGLE_framebuffer_blit

  -- * GL_ANGLE_framebuffer_blit
  , glBlitFramebufferANGLE
  , pattern GL_DRAW_FRAMEBUFFER_ANGLE
  , pattern GL_DRAW_FRAMEBUFFER_BINDING_ANGLE
  , pattern GL_READ_FRAMEBUFFER_ANGLE
  , pattern GL_READ_FRAMEBUFFER_BINDING_ANGLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_framebuffer_blit.txt GL_ANGLE_framebuffer_blit> extension is available.

gl_ANGLE_framebuffer_blit :: Bool
gl_ANGLE_framebuffer_blit = member "GL_ANGLE_framebuffer_blit" extensions
{-# NOINLINE gl_ANGLE_framebuffer_blit #-}

-- | Usage: @'glBlitFramebufferANGLE' srcX0 srcY0 srcX1 srcY1 dstX0 dstY0 dstX1 dstY1 mask filter@


glBlitFramebufferANGLE :: MonadIO m => GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> m ()
glBlitFramebufferANGLE = ffiintintintintintintintintbitfieldenumIOV glBlitFramebufferANGLEFunPtr

glBlitFramebufferANGLEFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
glBlitFramebufferANGLEFunPtr = unsafePerformIO (getProcAddress "glBlitFramebufferANGLE")

{-# NOINLINE glBlitFramebufferANGLEFunPtr #-}

pattern GL_DRAW_FRAMEBUFFER_ANGLE  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_ANGLE = 0x8CA9

pattern GL_DRAW_FRAMEBUFFER_BINDING_ANGLE  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_BINDING_ANGLE = 0x8CA6

pattern GL_READ_FRAMEBUFFER_ANGLE  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_ANGLE = 0x8CA8

pattern GL_READ_FRAMEBUFFER_BINDING_ANGLE  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_BINDING_ANGLE = 0x8CAA