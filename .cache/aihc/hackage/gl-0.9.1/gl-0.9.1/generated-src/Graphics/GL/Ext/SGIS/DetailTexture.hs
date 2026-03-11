-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.DetailTexture (
  -- * Extension Support
    gl_SGIS_detail_texture

  -- * GL_SGIS_detail_texture
  , glDetailTexFuncSGIS
  , glGetDetailTexFuncSGIS
  , pattern GL_DETAIL_TEXTURE_2D_BINDING_SGIS
  , pattern GL_DETAIL_TEXTURE_2D_SGIS
  , pattern GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS
  , pattern GL_DETAIL_TEXTURE_LEVEL_SGIS
  , pattern GL_DETAIL_TEXTURE_MODE_SGIS
  , pattern GL_LINEAR_DETAIL_ALPHA_SGIS
  , pattern GL_LINEAR_DETAIL_COLOR_SGIS
  , pattern GL_LINEAR_DETAIL_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/detail_texture.txt GL_SGIS_detail_texture> extension is available.

gl_SGIS_detail_texture :: Bool
gl_SGIS_detail_texture = member "GL_SGIS_detail_texture" extensions
{-# NOINLINE gl_SGIS_detail_texture #-}

-- | Usage: @'glDetailTexFuncSGIS' target n points@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The length of @points@ should be @n*2@.


glDetailTexFuncSGIS :: MonadIO m => GLenum -> GLsizei -> Ptr GLfloat -> m ()
glDetailTexFuncSGIS = ffienumsizeiPtrfloatIOV glDetailTexFuncSGISFunPtr

glDetailTexFuncSGISFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
glDetailTexFuncSGISFunPtr = unsafePerformIO (getProcAddress "glDetailTexFuncSGIS")

{-# NOINLINE glDetailTexFuncSGISFunPtr #-}

-- | Usage: @'glGetDetailTexFuncSGIS' target points@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The length of @points@ should be @COMPSIZE(target)@.


glGetDetailTexFuncSGIS :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glGetDetailTexFuncSGIS = ffienumPtrfloatIOV glGetDetailTexFuncSGISFunPtr

glGetDetailTexFuncSGISFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glGetDetailTexFuncSGISFunPtr = unsafePerformIO (getProcAddress "glGetDetailTexFuncSGIS")

{-# NOINLINE glGetDetailTexFuncSGISFunPtr #-}

pattern GL_DETAIL_TEXTURE_2D_BINDING_SGIS  :: (Eq a, Num a) => a

pattern GL_DETAIL_TEXTURE_2D_BINDING_SGIS = 0x8096

pattern GL_DETAIL_TEXTURE_2D_SGIS  :: (Eq a, Num a) => a

pattern GL_DETAIL_TEXTURE_2D_SGIS = 0x8095

pattern GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS  :: (Eq a, Num a) => a

pattern GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS = 0x809C

pattern GL_DETAIL_TEXTURE_LEVEL_SGIS  :: (Eq a, Num a) => a

pattern GL_DETAIL_TEXTURE_LEVEL_SGIS = 0x809A

pattern GL_DETAIL_TEXTURE_MODE_SGIS  :: (Eq a, Num a) => a

pattern GL_DETAIL_TEXTURE_MODE_SGIS = 0x809B

pattern GL_LINEAR_DETAIL_ALPHA_SGIS  :: (Eq a, Num a) => a

pattern GL_LINEAR_DETAIL_ALPHA_SGIS = 0x8098

pattern GL_LINEAR_DETAIL_COLOR_SGIS  :: (Eq a, Num a) => a

pattern GL_LINEAR_DETAIL_COLOR_SGIS = 0x8099

pattern GL_LINEAR_DETAIL_SGIS  :: (Eq a, Num a) => a

pattern GL_LINEAR_DETAIL_SGIS = 0x8097