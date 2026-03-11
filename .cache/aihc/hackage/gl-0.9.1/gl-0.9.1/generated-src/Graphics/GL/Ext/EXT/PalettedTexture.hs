-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PalettedTexture (
  -- * Extension Support
    gl_EXT_paletted_texture

  -- * GL_EXT_paletted_texture
  , glColorTableEXT
  , glGetColorTableEXT
  , glGetColorTableParameterfvEXT
  , glGetColorTableParameterivEXT
  , pattern GL_COLOR_INDEX12_EXT
  , pattern GL_COLOR_INDEX16_EXT
  , pattern GL_COLOR_INDEX1_EXT
  , pattern GL_COLOR_INDEX2_EXT
  , pattern GL_COLOR_INDEX4_EXT
  , pattern GL_COLOR_INDEX8_EXT
  , pattern GL_TEXTURE_INDEX_SIZE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/paletted_texture.txt GL_EXT_paletted_texture> extension is available.

gl_EXT_paletted_texture :: Bool
gl_EXT_paletted_texture = member "GL_EXT_paletted_texture" extensions
{-# NOINLINE gl_EXT_paletted_texture #-}

-- | Usage: @'glColorTableEXT' target internalFormat width format type table@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @internalFormat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @table@ should be @COMPSIZE(format,type,width)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glColorTable'.


glColorTableEXT :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glColorTableEXT = ffienumenumsizeienumenumPtrVIOV glColorTableEXTFunPtr

glColorTableEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glColorTableEXTFunPtr = unsafePerformIO (getProcAddress "glColorTableEXT")

{-# NOINLINE glColorTableEXTFunPtr #-}

-- | Usage: @'glGetColorTableEXT' target format type data@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @data@ should be @COMPSIZE(target,format,type)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glGetColorTable'.


glGetColorTableEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr () -> m ()
glGetColorTableEXT = ffienumenumenumPtrVIOV glGetColorTableEXTFunPtr

glGetColorTableEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> IO ())
glGetColorTableEXTFunPtr = unsafePerformIO (getProcAddress "glGetColorTableEXT")

{-# NOINLINE glGetColorTableEXTFunPtr #-}

-- | Usage: @'glGetColorTableParameterfvEXT' target pname params@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @pname@ is a @GetColorTableParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glGetColorTableParameterfv'.


glGetColorTableParameterfvEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetColorTableParameterfvEXT = ffienumenumPtrfloatIOV glGetColorTableParameterfvEXTFunPtr

glGetColorTableParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetColorTableParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetColorTableParameterfvEXT")

{-# NOINLINE glGetColorTableParameterfvEXTFunPtr #-}

-- | Usage: @'glGetColorTableParameterivEXT' target pname params@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @pname@ is a @GetColorTableParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glGetColorTableParameteriv'.


glGetColorTableParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetColorTableParameterivEXT = ffienumenumPtrintIOV glGetColorTableParameterivEXTFunPtr

glGetColorTableParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetColorTableParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetColorTableParameterivEXT")

{-# NOINLINE glGetColorTableParameterivEXTFunPtr #-}

pattern GL_COLOR_INDEX12_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_INDEX12_EXT = 0x80E6

pattern GL_COLOR_INDEX16_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_INDEX16_EXT = 0x80E7

pattern GL_COLOR_INDEX1_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_INDEX1_EXT = 0x80E2

pattern GL_COLOR_INDEX2_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_INDEX2_EXT = 0x80E3

pattern GL_COLOR_INDEX4_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_INDEX4_EXT = 0x80E4

pattern GL_COLOR_INDEX8_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_INDEX8_EXT = 0x80E5

pattern GL_TEXTURE_INDEX_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_INDEX_SIZE_EXT = 0x80ED