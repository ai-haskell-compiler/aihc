-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.TextureFilter4 (
  -- * Extension Support
    gl_SGIS_texture_filter4

  -- * GL_SGIS_texture_filter4
  , glGetTexFilterFuncSGIS
  , glTexFilterFuncSGIS
  , pattern GL_FILTER4_SGIS
  , pattern GL_TEXTURE_FILTER4_SIZE_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/texture_filter4.txt GL_SGIS_texture_filter4> extension is available.

gl_SGIS_texture_filter4 :: Bool
gl_SGIS_texture_filter4 = member "GL_SGIS_texture_filter4" extensions
{-# NOINLINE gl_SGIS_texture_filter4 #-}

-- | Usage: @'glGetTexFilterFuncSGIS' target filter weights@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @filter@ is a @TextureFilterSGIS@.
--
-- The length of @weights@ should be @COMPSIZE(target,filter)@.


glGetTexFilterFuncSGIS :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetTexFilterFuncSGIS = ffienumenumPtrfloatIOV glGetTexFilterFuncSGISFunPtr

glGetTexFilterFuncSGISFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetTexFilterFuncSGISFunPtr = unsafePerformIO (getProcAddress "glGetTexFilterFuncSGIS")

{-# NOINLINE glGetTexFilterFuncSGISFunPtr #-}

-- | Usage: @'glTexFilterFuncSGIS' target filter n weights@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @filter@ is a @TextureFilterSGIS@.
--
-- The length of @weights@ should be @n@.


glTexFilterFuncSGIS :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> m ()
glTexFilterFuncSGIS = ffienumenumsizeiPtrfloatIOV glTexFilterFuncSGISFunPtr

glTexFilterFuncSGISFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ())
glTexFilterFuncSGISFunPtr = unsafePerformIO (getProcAddress "glTexFilterFuncSGIS")

{-# NOINLINE glTexFilterFuncSGISFunPtr #-}

pattern GL_FILTER4_SGIS  :: (Eq a, Num a) => a

pattern GL_FILTER4_SGIS = 0x8146

pattern GL_TEXTURE_FILTER4_SIZE_SGIS  :: (Eq a, Num a) => a

pattern GL_TEXTURE_FILTER4_SIZE_SGIS = 0x8147