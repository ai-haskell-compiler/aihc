-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.CopyBuffer (
  -- * Extension Support
    gl_NV_copy_buffer

  -- * GL_NV_copy_buffer
  , glCopyBufferSubDataNV
  , pattern GL_COPY_READ_BUFFER_NV
  , pattern GL_COPY_WRITE_BUFFER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_copy_buffer.txt GL_NV_copy_buffer> extension is available.

gl_NV_copy_buffer :: Bool
gl_NV_copy_buffer = member "GL_NV_copy_buffer" extensions
{-# NOINLINE gl_NV_copy_buffer #-}

-- | Usage: @'glCopyBufferSubDataNV' readTarget writeTarget readOffset writeOffset size@
--
-- The parameter @readOffset@ is a @BufferOffset@.
--
-- The parameter @writeOffset@ is a @BufferOffset@.
--
-- The parameter @size@ is a @BufferSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyBufferSubData'.


glCopyBufferSubDataNV :: MonadIO m => GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> m ()
glCopyBufferSubDataNV = ffienumenumintptrintptrsizeiptrIOV glCopyBufferSubDataNVFunPtr

glCopyBufferSubDataNVFunPtr :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
glCopyBufferSubDataNVFunPtr = unsafePerformIO (getProcAddress "glCopyBufferSubDataNV")

{-# NOINLINE glCopyBufferSubDataNVFunPtr #-}

pattern GL_COPY_READ_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_COPY_READ_BUFFER_NV = 0x8F36

pattern GL_COPY_WRITE_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_COPY_WRITE_BUFFER_NV = 0x8F37