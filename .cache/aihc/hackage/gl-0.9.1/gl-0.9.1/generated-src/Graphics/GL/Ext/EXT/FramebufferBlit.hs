-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FramebufferBlit (
  -- * Extension Support
    gl_EXT_framebuffer_blit

  -- * GL_EXT_framebuffer_blit
  , glBlitFramebufferEXT
  , pattern GL_DRAW_FRAMEBUFFER_BINDING_EXT
  , pattern GL_DRAW_FRAMEBUFFER_EXT
  , pattern GL_READ_FRAMEBUFFER_BINDING_EXT
  , pattern GL_READ_FRAMEBUFFER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/framebuffer_blit.txt GL_EXT_framebuffer_blit> extension is available.

gl_EXT_framebuffer_blit :: Bool
gl_EXT_framebuffer_blit = member "GL_EXT_framebuffer_blit" extensions
{-# NOINLINE gl_EXT_framebuffer_blit #-}

-- | Usage: @'glBlitFramebufferEXT' srcX0 srcY0 srcX1 srcY1 dstX0 dstY0 dstX1 dstY1 mask filter@
--
-- The parameter @mask@ is a @ClearBufferMask@, one of: 'Graphics.GL.Internal.Shared.GL_ACCUM_BUFFER_BIT', 'Graphics.GL.Internal.Shared.GL_COLOR_BUFFER_BIT', 'Graphics.GL.Ext.NV.CoverageSample.GL_COVERAGE_BUFFER_BIT_NV', 'Graphics.GL.Internal.Shared.GL_DEPTH_BUFFER_BIT', 'Graphics.GL.Internal.Shared.GL_STENCIL_BUFFER_BIT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlitFramebuffer'.


glBlitFramebufferEXT :: MonadIO m => GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> m ()
glBlitFramebufferEXT = ffiintintintintintintintintbitfieldenumIOV glBlitFramebufferEXTFunPtr

glBlitFramebufferEXTFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
glBlitFramebufferEXTFunPtr = unsafePerformIO (getProcAddress "glBlitFramebufferEXT")

{-# NOINLINE glBlitFramebufferEXTFunPtr #-}

pattern GL_DRAW_FRAMEBUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_BINDING_EXT = 0x8CA6

pattern GL_DRAW_FRAMEBUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_FRAMEBUFFER_EXT = 0x8CA9

pattern GL_READ_FRAMEBUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_BINDING_EXT = 0x8CAA

pattern GL_READ_FRAMEBUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_READ_FRAMEBUFFER_EXT = 0x8CA8