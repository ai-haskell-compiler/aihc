-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureBufferObject (
  -- * Extension Support
    gl_ARB_texture_buffer_object

  -- * GL_ARB_texture_buffer_object
  , glTexBufferARB
  , pattern GL_MAX_TEXTURE_BUFFER_SIZE_ARB
  , pattern GL_TEXTURE_BINDING_BUFFER_ARB
  , pattern GL_TEXTURE_BUFFER_ARB
  , pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB
  , pattern GL_TEXTURE_BUFFER_FORMAT_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_buffer_object.txt GL_ARB_texture_buffer_object> extension is available.

gl_ARB_texture_buffer_object :: Bool
gl_ARB_texture_buffer_object = member "GL_ARB_texture_buffer_object" extensions
{-# NOINLINE gl_ARB_texture_buffer_object #-}

-- | Usage: @'glTexBufferARB' target internalformat buffer@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexBuffer'.


glTexBufferARB :: MonadIO m => GLenum -> GLenum -> GLuint -> m ()
glTexBufferARB = ffienumenumuintIOV glTexBufferARBFunPtr

glTexBufferARBFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
glTexBufferARBFunPtr = unsafePerformIO (getProcAddress "glTexBufferARB")

{-# NOINLINE glTexBufferARBFunPtr #-}

pattern GL_MAX_TEXTURE_BUFFER_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_TEXTURE_BUFFER_SIZE_ARB = 0x8C2B

pattern GL_TEXTURE_BINDING_BUFFER_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_BUFFER_ARB = 0x8C2C

pattern GL_TEXTURE_BUFFER_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_ARB = 0x8C2A

pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB = 0x8C2D

pattern GL_TEXTURE_BUFFER_FORMAT_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_FORMAT_ARB = 0x8C2E