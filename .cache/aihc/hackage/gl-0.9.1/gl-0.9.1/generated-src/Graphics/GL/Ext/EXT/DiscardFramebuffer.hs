-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DiscardFramebuffer (
  -- * Extension Support
    gl_EXT_discard_framebuffer

  -- * GL_EXT_discard_framebuffer
  , glDiscardFramebufferEXT
  , pattern GL_COLOR_EXT
  , pattern GL_DEPTH_EXT
  , pattern GL_STENCIL_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_discard_framebuffer.txt GL_EXT_discard_framebuffer> extension is available.

gl_EXT_discard_framebuffer :: Bool
gl_EXT_discard_framebuffer = member "GL_EXT_discard_framebuffer" extensions
{-# NOINLINE gl_EXT_discard_framebuffer #-}

-- | Usage: @'glDiscardFramebufferEXT' target numAttachments attachments@
--
-- The length of @attachments@ should be @numAttachments@.


glDiscardFramebufferEXT :: MonadIO m => GLenum -> GLsizei -> Ptr GLenum -> m ()
glDiscardFramebufferEXT = ffienumsizeiPtrenumIOV glDiscardFramebufferEXTFunPtr

glDiscardFramebufferEXTFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLenum -> IO ())
glDiscardFramebufferEXTFunPtr = unsafePerformIO (getProcAddress "glDiscardFramebufferEXT")

{-# NOINLINE glDiscardFramebufferEXTFunPtr #-}

pattern GL_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_EXT = 0x1800

pattern GL_DEPTH_EXT  :: (Eq a, Num a) => a

pattern GL_DEPTH_EXT = 0x1801

pattern GL_STENCIL_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_EXT = 0x1802