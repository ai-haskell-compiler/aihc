-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.CopyTexture (
  -- * Extension Support
    gl_EXT_copy_texture

  -- * GL_EXT_copy_texture
  , glCopyTexImage1DEXT
  , glCopyTexImage2DEXT
  , glCopyTexSubImage1DEXT
  , glCopyTexSubImage2DEXT
  , glCopyTexSubImage3DEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/copy_texture.txt GL_EXT_copy_texture> extension is available.

gl_EXT_copy_texture :: Bool
gl_EXT_copy_texture = member "GL_EXT_copy_texture" extensions
{-# NOINLINE gl_EXT_copy_texture #-}

-- | Usage: @'glCopyTexImage1DEXT' target level internalformat x y width border@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyTexImage1D'.


glCopyTexImage1DEXT :: MonadIO m => GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> m ()
glCopyTexImage1DEXT = ffienumintenumintintsizeiintIOV glCopyTexImage1DEXTFunPtr

glCopyTexImage1DEXTFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
glCopyTexImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTexImage1DEXT")

{-# NOINLINE glCopyTexImage1DEXTFunPtr #-}

-- | Usage: @'glCopyTexImage2DEXT' target level internalformat x y width height border@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyTexImage2D'.


glCopyTexImage2DEXT :: MonadIO m => GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> m ()
glCopyTexImage2DEXT = ffienumintenumintintsizeisizeiintIOV glCopyTexImage2DEXTFunPtr

glCopyTexImage2DEXTFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
glCopyTexImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTexImage2DEXT")

{-# NOINLINE glCopyTexImage2DEXTFunPtr #-}

-- | Usage: @'glCopyTexSubImage1DEXT' target level xoffset x y width@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyTexSubImage1D'.


glCopyTexSubImage1DEXT :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> m ()
glCopyTexSubImage1DEXT = ffienumintintintintsizeiIOV glCopyTexSubImage1DEXTFunPtr

glCopyTexSubImage1DEXTFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
glCopyTexSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTexSubImage1DEXT")

{-# NOINLINE glCopyTexSubImage1DEXTFunPtr #-}

-- | Usage: @'glCopyTexSubImage2DEXT' target level xoffset yoffset x y width height@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyTexSubImage2D'.


glCopyTexSubImage2DEXT :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyTexSubImage2DEXT = ffienumintintintintintsizeisizeiIOV glCopyTexSubImage2DEXTFunPtr

glCopyTexSubImage2DEXTFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyTexSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTexSubImage2DEXT")

{-# NOINLINE glCopyTexSubImage2DEXTFunPtr #-}

-- | Usage: @'glCopyTexSubImage3DEXT' target level xoffset yoffset zoffset x y width height@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyTexSubImage3D'.


glCopyTexSubImage3DEXT :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyTexSubImage3DEXT = ffienumintintintintintintsizeisizeiIOV glCopyTexSubImage3DEXTFunPtr

glCopyTexSubImage3DEXTFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyTexSubImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTexSubImage3DEXT")

{-# NOINLINE glCopyTexSubImage3DEXTFunPtr #-}