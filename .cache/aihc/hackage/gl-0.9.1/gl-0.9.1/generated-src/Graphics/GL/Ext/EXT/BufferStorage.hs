-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BufferStorage (
  -- * Extension Support
    gl_EXT_buffer_storage

  -- * GL_EXT_buffer_storage
  , glBufferStorageEXT
  , pattern GL_BUFFER_IMMUTABLE_STORAGE_EXT
  , pattern GL_BUFFER_STORAGE_FLAGS_EXT
  , pattern GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT_EXT
  , pattern GL_CLIENT_STORAGE_BIT_EXT
  , pattern GL_DYNAMIC_STORAGE_BIT_EXT
  , pattern GL_MAP_COHERENT_BIT_EXT
  , pattern GL_MAP_PERSISTENT_BIT_EXT
  , pattern GL_MAP_READ_BIT
  , pattern GL_MAP_WRITE_BIT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_buffer_storage.txt GL_EXT_buffer_storage> extension is available.

gl_EXT_buffer_storage :: Bool
gl_EXT_buffer_storage = member "GL_EXT_buffer_storage" extensions
{-# NOINLINE gl_EXT_buffer_storage #-}

-- | Usage: @'glBufferStorageEXT' target size data flags@
--
-- The length of @data@ should be @size@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBufferStorage'.


glBufferStorageEXT :: MonadIO m => GLenum -> GLsizeiptr -> Ptr () -> GLbitfield -> m ()
glBufferStorageEXT = ffienumsizeiptrPtrVbitfieldIOV glBufferStorageEXTFunPtr

glBufferStorageEXTFunPtr :: FunPtr (GLenum -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ())
glBufferStorageEXTFunPtr = unsafePerformIO (getProcAddress "glBufferStorageEXT")

{-# NOINLINE glBufferStorageEXTFunPtr #-}

pattern GL_BUFFER_IMMUTABLE_STORAGE_EXT  :: (Eq a, Num a) => a

pattern GL_BUFFER_IMMUTABLE_STORAGE_EXT = 0x821F

pattern GL_BUFFER_STORAGE_FLAGS_EXT  :: (Eq a, Num a) => a

pattern GL_BUFFER_STORAGE_FLAGS_EXT = 0x8220

pattern GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT_EXT = 0x00004000

pattern GL_CLIENT_STORAGE_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_CLIENT_STORAGE_BIT_EXT = 0x0200

pattern GL_DYNAMIC_STORAGE_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_DYNAMIC_STORAGE_BIT_EXT = 0x0100

pattern GL_MAP_COHERENT_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_COHERENT_BIT_EXT = 0x0080

pattern GL_MAP_PERSISTENT_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_PERSISTENT_BIT_EXT = 0x0040