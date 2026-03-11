-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Subtexture (
  -- * Extension Support
    gl_EXT_subtexture

  -- * GL_EXT_subtexture
  , glTexSubImage1DEXT
  , glTexSubImage2DEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/subtexture.txt GL_EXT_subtexture> extension is available.

gl_EXT_subtexture :: Bool
gl_EXT_subtexture = member "GL_EXT_subtexture" extensions
{-# NOINLINE gl_EXT_subtexture #-}

-- | Usage: @'glTexSubImage1DEXT' target level xoffset width format type pixels@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexSubImage1D'.


glTexSubImage1DEXT :: MonadIO m => GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glTexSubImage1DEXT = ffienumintintsizeienumenumPtrVIOV glTexSubImage1DEXTFunPtr

glTexSubImage1DEXTFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glTexSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glTexSubImage1DEXT")

{-# NOINLINE glTexSubImage1DEXTFunPtr #-}

-- | Usage: @'glTexSubImage2DEXT' target level xoffset yoffset width height format type pixels@
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexSubImage2D'.


glTexSubImage2DEXT :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glTexSubImage2DEXT = ffienumintintintsizeisizeienumenumPtrVIOV glTexSubImage2DEXTFunPtr

glTexSubImage2DEXTFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glTexSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glTexSubImage2DEXT")

{-# NOINLINE glTexSubImage2DEXTFunPtr #-}