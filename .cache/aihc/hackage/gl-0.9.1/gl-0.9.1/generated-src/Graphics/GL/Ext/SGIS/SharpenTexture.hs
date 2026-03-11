-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.SharpenTexture (
  -- * Extension Support
    gl_SGIS_sharpen_texture

  -- * GL_SGIS_sharpen_texture
  , glGetSharpenTexFuncSGIS
  , glSharpenTexFuncSGIS
  , pattern GL_LINEAR_SHARPEN_ALPHA_SGIS
  , pattern GL_LINEAR_SHARPEN_COLOR_SGIS
  , pattern GL_LINEAR_SHARPEN_SGIS
  , pattern GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/sharpen_texture.txt GL_SGIS_sharpen_texture> extension is available.

gl_SGIS_sharpen_texture :: Bool
gl_SGIS_sharpen_texture = member "GL_SGIS_sharpen_texture" extensions
{-# NOINLINE gl_SGIS_sharpen_texture #-}

-- | Usage: @'glGetSharpenTexFuncSGIS' target points@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The length of @points@ should be @COMPSIZE(target)@.


glGetSharpenTexFuncSGIS :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glGetSharpenTexFuncSGIS = ffienumPtrfloatIOV glGetSharpenTexFuncSGISFunPtr

glGetSharpenTexFuncSGISFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glGetSharpenTexFuncSGISFunPtr = unsafePerformIO (getProcAddress "glGetSharpenTexFuncSGIS")

{-# NOINLINE glGetSharpenTexFuncSGISFunPtr #-}

-- | Usage: @'glSharpenTexFuncSGIS' target n points@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The length of @points@ should be @n*2@.


glSharpenTexFuncSGIS :: MonadIO m => GLenum -> GLsizei -> Ptr GLfloat -> m ()
glSharpenTexFuncSGIS = ffienumsizeiPtrfloatIOV glSharpenTexFuncSGISFunPtr

glSharpenTexFuncSGISFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
glSharpenTexFuncSGISFunPtr = unsafePerformIO (getProcAddress "glSharpenTexFuncSGIS")

{-# NOINLINE glSharpenTexFuncSGISFunPtr #-}

pattern GL_LINEAR_SHARPEN_ALPHA_SGIS  :: (Eq a, Num a) => a

pattern GL_LINEAR_SHARPEN_ALPHA_SGIS = 0x80AE

pattern GL_LINEAR_SHARPEN_COLOR_SGIS  :: (Eq a, Num a) => a

pattern GL_LINEAR_SHARPEN_COLOR_SGIS = 0x80AF

pattern GL_LINEAR_SHARPEN_SGIS  :: (Eq a, Num a) => a

pattern GL_LINEAR_SHARPEN_SGIS = 0x80AD

pattern GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS  :: (Eq a, Num a) => a

pattern GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS = 0x80B0