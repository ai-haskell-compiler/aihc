-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureBuffer (
  -- * Extension Support
    gl_EXT_texture_buffer

  -- * GL_EXT_texture_buffer
  , glTexBufferEXT
  , glTexBufferRangeEXT
  , pattern GL_IMAGE_BUFFER_EXT
  , pattern GL_INT_IMAGE_BUFFER_EXT
  , pattern GL_INT_SAMPLER_BUFFER_EXT
  , pattern GL_MAX_TEXTURE_BUFFER_SIZE_EXT
  , pattern GL_SAMPLER_BUFFER_EXT
  , pattern GL_TEXTURE_BINDING_BUFFER_EXT
  , pattern GL_TEXTURE_BUFFER_BINDING_EXT
  , pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT
  , pattern GL_TEXTURE_BUFFER_EXT
  , pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT_EXT
  , pattern GL_TEXTURE_BUFFER_OFFSET_EXT
  , pattern GL_TEXTURE_BUFFER_SIZE_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_BUFFER_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_buffer.txt GL_EXT_texture_buffer> extension is available.

gl_EXT_texture_buffer :: Bool
gl_EXT_texture_buffer = member "GL_EXT_texture_buffer" extensions
{-# NOINLINE gl_EXT_texture_buffer #-}

-- | Usage: @'glTexBufferRangeEXT' target internalformat buffer offset size@
--
-- The parameter @offset@ is a @BufferOffset@.
--
-- The parameter @size@ is a @BufferSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexBufferRange'.


glTexBufferRangeEXT :: MonadIO m => GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> m ()
glTexBufferRangeEXT = ffienumenumuintintptrsizeiptrIOV glTexBufferRangeEXTFunPtr

glTexBufferRangeEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
glTexBufferRangeEXTFunPtr = unsafePerformIO (getProcAddress "glTexBufferRangeEXT")

{-# NOINLINE glTexBufferRangeEXTFunPtr #-}

pattern GL_TEXTURE_BUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_BINDING_EXT = 0x8C2A

pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT_EXT = 0x919F

pattern GL_TEXTURE_BUFFER_OFFSET_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_OFFSET_EXT = 0x919D

pattern GL_TEXTURE_BUFFER_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_SIZE_EXT = 0x919E