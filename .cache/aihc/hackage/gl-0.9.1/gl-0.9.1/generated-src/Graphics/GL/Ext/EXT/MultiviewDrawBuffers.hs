-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.MultiviewDrawBuffers (
  -- * Extension Support
    gl_EXT_multiview_draw_buffers

  -- * GL_EXT_multiview_draw_buffers
  , glDrawBuffersIndexedEXT
  , glGetIntegeri_vEXT
  , glReadBufferIndexedEXT
  , pattern GL_COLOR_ATTACHMENT_EXT
  , pattern GL_DRAW_BUFFER_EXT
  , pattern GL_MAX_MULTIVIEW_BUFFERS_EXT
  , pattern GL_MULTIVIEW_EXT
  , pattern GL_READ_BUFFER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_multiview_draw_buffers.txt GL_EXT_multiview_draw_buffers> extension is available.

gl_EXT_multiview_draw_buffers :: Bool
gl_EXT_multiview_draw_buffers = member "GL_EXT_multiview_draw_buffers" extensions
{-# NOINLINE gl_EXT_multiview_draw_buffers #-}

-- | Usage: @'glDrawBuffersIndexedEXT' n location indices@
--
-- The length of @location@ should be @n@.
--
-- The length of @indices@ should be @n@.


glDrawBuffersIndexedEXT :: MonadIO m => GLint -> Ptr GLenum -> Ptr GLint -> m ()
glDrawBuffersIndexedEXT = ffiintPtrenumPtrintIOV glDrawBuffersIndexedEXTFunPtr

glDrawBuffersIndexedEXTFunPtr :: FunPtr (GLint -> Ptr GLenum -> Ptr GLint -> IO ())
glDrawBuffersIndexedEXTFunPtr = unsafePerformIO (getProcAddress "glDrawBuffersIndexedEXT")

{-# NOINLINE glDrawBuffersIndexedEXTFunPtr #-}

-- | Usage: @'glGetIntegeri_vEXT' target index data@


glGetIntegeri_vEXT :: MonadIO m => GLenum -> GLuint -> Ptr GLint -> m ()
glGetIntegeri_vEXT = ffienumuintPtrintIOV glGetIntegeri_vEXTFunPtr

glGetIntegeri_vEXTFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
glGetIntegeri_vEXTFunPtr = unsafePerformIO (getProcAddress "glGetIntegeri_vEXT")

{-# NOINLINE glGetIntegeri_vEXTFunPtr #-}

-- | Usage: @'glReadBufferIndexedEXT' src index@


glReadBufferIndexedEXT :: MonadIO m => GLenum -> GLint -> m ()
glReadBufferIndexedEXT = ffienumintIOV glReadBufferIndexedEXTFunPtr

glReadBufferIndexedEXTFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glReadBufferIndexedEXTFunPtr = unsafePerformIO (getProcAddress "glReadBufferIndexedEXT")

{-# NOINLINE glReadBufferIndexedEXTFunPtr #-}

pattern GL_COLOR_ATTACHMENT_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_ATTACHMENT_EXT = 0x90F0

pattern GL_DRAW_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER_EXT = 0x0C01

pattern GL_MAX_MULTIVIEW_BUFFERS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_MULTIVIEW_BUFFERS_EXT = 0x90F2

pattern GL_MULTIVIEW_EXT  :: (Eq a, Num a) => a

pattern GL_MULTIVIEW_EXT = 0x90F1

pattern GL_READ_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_READ_BUFFER_EXT = 0x0C02