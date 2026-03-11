-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureBorderClamp (
  -- * Extension Support
    gl_OES_texture_border_clamp

  -- * GL_OES_texture_border_clamp
  , glGetSamplerParameterIivOES
  , glGetSamplerParameterIuivOES
  , glGetTexParameterIivOES
  , glGetTexParameterIuivOES
  , glSamplerParameterIivOES
  , glSamplerParameterIuivOES
  , glTexParameterIivOES
  , glTexParameterIuivOES
  , pattern GL_CLAMP_TO_BORDER_OES
  , pattern GL_TEXTURE_BORDER_COLOR_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_border_clamp.txt GL_OES_texture_border_clamp> extension is available.

gl_OES_texture_border_clamp :: Bool
gl_OES_texture_border_clamp = member "GL_OES_texture_border_clamp" extensions
{-# NOINLINE gl_OES_texture_border_clamp #-}

-- | Usage: @'glGetSamplerParameterIivOES' sampler pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetSamplerParameterIiv'.


glGetSamplerParameterIivOES :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetSamplerParameterIivOES = ffiuintenumPtrintIOV glGetSamplerParameterIivOESFunPtr

glGetSamplerParameterIivOESFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetSamplerParameterIivOESFunPtr = unsafePerformIO (getProcAddress "glGetSamplerParameterIivOES")

{-# NOINLINE glGetSamplerParameterIivOESFunPtr #-}

-- | Usage: @'glGetSamplerParameterIuivOES' sampler pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetSamplerParameterIuiv'.


glGetSamplerParameterIuivOES :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glGetSamplerParameterIuivOES = ffiuintenumPtruintIOV glGetSamplerParameterIuivOESFunPtr

glGetSamplerParameterIuivOESFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glGetSamplerParameterIuivOESFunPtr = unsafePerformIO (getProcAddress "glGetSamplerParameterIuivOES")

{-# NOINLINE glGetSamplerParameterIuivOESFunPtr #-}

-- | Usage: @'glGetTexParameterIivOES' target pname params@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetTexParameterIiv'.


glGetTexParameterIivOES :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetTexParameterIivOES = ffienumenumPtrintIOV glGetTexParameterIivOESFunPtr

glGetTexParameterIivOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetTexParameterIivOESFunPtr = unsafePerformIO (getProcAddress "glGetTexParameterIivOES")

{-# NOINLINE glGetTexParameterIivOESFunPtr #-}

-- | Usage: @'glGetTexParameterIuivOES' target pname params@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetTexParameterIuiv'.


glGetTexParameterIuivOES :: MonadIO m => GLenum -> GLenum -> Ptr GLuint -> m ()
glGetTexParameterIuivOES = ffienumenumPtruintIOV glGetTexParameterIuivOESFunPtr

glGetTexParameterIuivOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
glGetTexParameterIuivOESFunPtr = unsafePerformIO (getProcAddress "glGetTexParameterIuivOES")

{-# NOINLINE glGetTexParameterIuivOESFunPtr #-}

-- | Usage: @'glSamplerParameterIivOES' sampler pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSamplerParameterIiv'.


glSamplerParameterIivOES :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glSamplerParameterIivOES = ffiuintenumPtrintIOV glSamplerParameterIivOESFunPtr

glSamplerParameterIivOESFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glSamplerParameterIivOESFunPtr = unsafePerformIO (getProcAddress "glSamplerParameterIivOES")

{-# NOINLINE glSamplerParameterIivOESFunPtr #-}

-- | Usage: @'glSamplerParameterIuivOES' sampler pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSamplerParameterIuiv'.


glSamplerParameterIuivOES :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glSamplerParameterIuivOES = ffiuintenumPtruintIOV glSamplerParameterIuivOESFunPtr

glSamplerParameterIuivOESFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glSamplerParameterIuivOESFunPtr = unsafePerformIO (getProcAddress "glSamplerParameterIuivOES")

{-# NOINLINE glSamplerParameterIuivOESFunPtr #-}

-- | Usage: @'glTexParameterIivOES' target pname params@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexParameterIiv'.


glTexParameterIivOES :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glTexParameterIivOES = ffienumenumPtrintIOV glTexParameterIivOESFunPtr

glTexParameterIivOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glTexParameterIivOESFunPtr = unsafePerformIO (getProcAddress "glTexParameterIivOES")

{-# NOINLINE glTexParameterIivOESFunPtr #-}

-- | Usage: @'glTexParameterIuivOES' target pname params@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexParameterIuiv'.


glTexParameterIuivOES :: MonadIO m => GLenum -> GLenum -> Ptr GLuint -> m ()
glTexParameterIuivOES = ffienumenumPtruintIOV glTexParameterIuivOESFunPtr

glTexParameterIuivOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
glTexParameterIuivOESFunPtr = unsafePerformIO (getProcAddress "glTexParameterIuivOES")

{-# NOINLINE glTexParameterIuivOESFunPtr #-}

pattern GL_CLAMP_TO_BORDER_OES  :: (Eq a, Num a) => a

pattern GL_CLAMP_TO_BORDER_OES = 0x812D

pattern GL_TEXTURE_BORDER_COLOR_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BORDER_COLOR_OES = 0x1004