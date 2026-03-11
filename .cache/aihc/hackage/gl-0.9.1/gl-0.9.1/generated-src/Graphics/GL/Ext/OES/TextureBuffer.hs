-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureBuffer (
  -- * Extension Support
    gl_OES_texture_buffer

  -- * GL_OES_texture_buffer
  , glTexBufferOES
  , glTexBufferRangeOES
  , pattern GL_IMAGE_BUFFER_OES
  , pattern GL_INT_IMAGE_BUFFER_OES
  , pattern GL_INT_SAMPLER_BUFFER_OES
  , pattern GL_MAX_TEXTURE_BUFFER_SIZE_OES
  , pattern GL_SAMPLER_BUFFER_OES
  , pattern GL_TEXTURE_BINDING_BUFFER_OES
  , pattern GL_TEXTURE_BUFFER_BINDING_OES
  , pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_OES
  , pattern GL_TEXTURE_BUFFER_OES
  , pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT_OES
  , pattern GL_TEXTURE_BUFFER_OFFSET_OES
  , pattern GL_TEXTURE_BUFFER_SIZE_OES
  , pattern GL_UNSIGNED_INT_IMAGE_BUFFER_OES
  , pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_buffer.txt GL_OES_texture_buffer> extension is available.

gl_OES_texture_buffer :: Bool
gl_OES_texture_buffer = member "GL_OES_texture_buffer" extensions
{-# NOINLINE gl_OES_texture_buffer #-}

-- | Usage: @'glTexBufferOES' target internalformat buffer@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexBuffer'.


glTexBufferOES :: MonadIO m => GLenum -> GLenum -> GLuint -> m ()
glTexBufferOES = ffienumenumuintIOV glTexBufferOESFunPtr

glTexBufferOESFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
glTexBufferOESFunPtr = unsafePerformIO (getProcAddress "glTexBufferOES")

{-# NOINLINE glTexBufferOESFunPtr #-}

-- | Usage: @'glTexBufferRangeOES' target internalformat buffer offset size@
--
-- The parameter @offset@ is a @BufferOffset@.
--
-- The parameter @size@ is a @BufferSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexBufferRange'.


glTexBufferRangeOES :: MonadIO m => GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> m ()
glTexBufferRangeOES = ffienumenumuintintptrsizeiptrIOV glTexBufferRangeOESFunPtr

glTexBufferRangeOESFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
glTexBufferRangeOESFunPtr = unsafePerformIO (getProcAddress "glTexBufferRangeOES")

{-# NOINLINE glTexBufferRangeOESFunPtr #-}

pattern GL_IMAGE_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_IMAGE_BUFFER_OES = 0x9051

pattern GL_INT_IMAGE_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_BUFFER_OES = 0x905C

pattern GL_INT_SAMPLER_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_BUFFER_OES = 0x8DD0

pattern GL_MAX_TEXTURE_BUFFER_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_MAX_TEXTURE_BUFFER_SIZE_OES = 0x8C2B

pattern GL_SAMPLER_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_SAMPLER_BUFFER_OES = 0x8DC2

pattern GL_TEXTURE_BINDING_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_BUFFER_OES = 0x8C2C

pattern GL_TEXTURE_BUFFER_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_BINDING_OES = 0x8C2A

pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_OES = 0x8C2D

pattern GL_TEXTURE_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_OES = 0x8C2A

pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT_OES = 0x919F

pattern GL_TEXTURE_BUFFER_OFFSET_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_OFFSET_OES = 0x919D

pattern GL_TEXTURE_BUFFER_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_SIZE_OES = 0x919E

pattern GL_UNSIGNED_INT_IMAGE_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_BUFFER_OES = 0x9067

pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_OES  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_OES = 0x8DD8