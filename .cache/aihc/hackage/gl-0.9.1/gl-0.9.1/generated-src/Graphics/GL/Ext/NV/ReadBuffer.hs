-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ReadBuffer (
  -- * Extension Support
    gl_NV_read_buffer

  -- * GL_NV_read_buffer
  , glReadBufferNV
  , pattern GL_READ_BUFFER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_read_buffer.txt GL_NV_read_buffer> extension is available.

gl_NV_read_buffer :: Bool
gl_NV_read_buffer = member "GL_NV_read_buffer" extensions
{-# NOINLINE gl_NV_read_buffer #-}

-- | Usage: @'glReadBufferNV' mode@


glReadBufferNV :: MonadIO m => GLenum -> m ()
glReadBufferNV = ffienumIOV glReadBufferNVFunPtr

glReadBufferNVFunPtr :: FunPtr (GLenum -> IO ())
glReadBufferNVFunPtr = unsafePerformIO (getProcAddress "glReadBufferNV")

{-# NOINLINE glReadBufferNVFunPtr #-}

pattern GL_READ_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_READ_BUFFER_NV = 0x0C02