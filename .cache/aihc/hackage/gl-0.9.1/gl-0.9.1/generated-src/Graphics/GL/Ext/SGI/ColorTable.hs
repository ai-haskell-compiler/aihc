-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGI.ColorTable (
  -- * Extension Support
    gl_SGI_color_table

  -- * GL_SGI_color_table
  , glColorTableParameterfvSGI
  , glColorTableParameterivSGI
  , glColorTableSGI
  , glCopyColorTableSGI
  , glGetColorTableParameterfvSGI
  , glGetColorTableParameterivSGI
  , glGetColorTableSGI
  , pattern GL_COLOR_TABLE_ALPHA_SIZE_SGI
  , pattern GL_COLOR_TABLE_BIAS_SGI
  , pattern GL_COLOR_TABLE_BLUE_SIZE_SGI
  , pattern GL_COLOR_TABLE_FORMAT_SGI
  , pattern GL_COLOR_TABLE_GREEN_SIZE_SGI
  , pattern GL_COLOR_TABLE_INTENSITY_SIZE_SGI
  , pattern GL_COLOR_TABLE_LUMINANCE_SIZE_SGI
  , pattern GL_COLOR_TABLE_RED_SIZE_SGI
  , pattern GL_COLOR_TABLE_SCALE_SGI
  , pattern GL_COLOR_TABLE_SGI
  , pattern GL_COLOR_TABLE_WIDTH_SGI
  , pattern GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI
  , pattern GL_POST_CONVOLUTION_COLOR_TABLE_SGI
  , pattern GL_PROXY_COLOR_TABLE_SGI
  , pattern GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI
  , pattern GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGI/color_table.txt GL_SGI_color_table> extension is available.

gl_SGI_color_table :: Bool
gl_SGI_color_table = member "GL_SGI_color_table" extensions
{-# NOINLINE gl_SGI_color_table #-}

-- | Usage: @'glColorTableParameterfvSGI' target pname params@
--
-- The parameter @target@ is a @ColorTableTargetSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_PROXY_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI'.
--
-- The parameter @pname@ is a @ColorTableParameterPNameSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE_BIAS', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_BIAS_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE_SCALE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SCALE_SGI'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glColorTableParameterfv'.


glColorTableParameterfvSGI :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glColorTableParameterfvSGI = ffienumenumPtrfloatIOV glColorTableParameterfvSGIFunPtr

glColorTableParameterfvSGIFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glColorTableParameterfvSGIFunPtr = unsafePerformIO (getProcAddress "glColorTableParameterfvSGI")

{-# NOINLINE glColorTableParameterfvSGIFunPtr #-}

-- | Usage: @'glColorTableParameterivSGI' target pname params@
--
-- The parameter @target@ is a @ColorTableTargetSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_PROXY_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI'.
--
-- The parameter @pname@ is a @ColorTableParameterPNameSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE_BIAS', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_BIAS_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE_SCALE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SCALE_SGI'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glColorTableParameteriv'.


glColorTableParameterivSGI :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glColorTableParameterivSGI = ffienumenumPtrintIOV glColorTableParameterivSGIFunPtr

glColorTableParameterivSGIFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glColorTableParameterivSGIFunPtr = unsafePerformIO (getProcAddress "glColorTableParameterivSGI")

{-# NOINLINE glColorTableParameterivSGIFunPtr #-}

-- | Usage: @'glColorTableSGI' target internalformat width format type table@
--
-- The parameter @target@ is a @ColorTableTargetSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_PROXY_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @table@ should be @COMPSIZE(format,type,width)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glColorTable'.


glColorTableSGI :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glColorTableSGI = ffienumenumsizeienumenumPtrVIOV glColorTableSGIFunPtr

glColorTableSGIFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glColorTableSGIFunPtr = unsafePerformIO (getProcAddress "glColorTableSGI")

{-# NOINLINE glColorTableSGIFunPtr #-}

-- | Usage: @'glCopyColorTableSGI' target internalformat x y width@
--
-- The parameter @target@ is a @ColorTableTargetSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_PROXY_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glCopyColorTable'.


glCopyColorTableSGI :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> m ()
glCopyColorTableSGI = ffienumenumintintsizeiIOV glCopyColorTableSGIFunPtr

glCopyColorTableSGIFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
glCopyColorTableSGIFunPtr = unsafePerformIO (getProcAddress "glCopyColorTableSGI")

{-# NOINLINE glCopyColorTableSGIFunPtr #-}

-- | Usage: @'glGetColorTableParameterfvSGI' target pname params@
--
-- The parameter @target@ is a @ColorTableTargetSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_PROXY_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI'.
--
-- The parameter @pname@ is a @GetColorTableParameterPNameSGI@, one of: 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_ALPHA_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_BIAS_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_BLUE_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_FORMAT_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_GREEN_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_INTENSITY_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_LUMINANCE_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_RED_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SCALE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_WIDTH_SGI'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetColorTableParameterfvSGI :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetColorTableParameterfvSGI = ffienumenumPtrfloatIOV glGetColorTableParameterfvSGIFunPtr

glGetColorTableParameterfvSGIFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetColorTableParameterfvSGIFunPtr = unsafePerformIO (getProcAddress "glGetColorTableParameterfvSGI")

{-# NOINLINE glGetColorTableParameterfvSGIFunPtr #-}

-- | Usage: @'glGetColorTableParameterivSGI' target pname params@
--
-- The parameter @target@ is a @ColorTableTargetSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_PROXY_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI'.
--
-- The parameter @pname@ is a @GetColorTableParameterPNameSGI@, one of: 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_ALPHA_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_BIAS_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_BLUE_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_FORMAT_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_GREEN_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_INTENSITY_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_LUMINANCE_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_RED_SIZE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SCALE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_WIDTH_SGI'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetColorTableParameterivSGI :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetColorTableParameterivSGI = ffienumenumPtrintIOV glGetColorTableParameterivSGIFunPtr

glGetColorTableParameterivSGIFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetColorTableParameterivSGIFunPtr = unsafePerformIO (getProcAddress "glGetColorTableParameterivSGI")

{-# NOINLINE glGetColorTableParameterivSGIFunPtr #-}

-- | Usage: @'glGetColorTableSGI' target format type table@
--
-- The parameter @target@ is a @ColorTableTargetSGI@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_PROXY_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI'.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @table@ should be @COMPSIZE(target,format,type)@.


glGetColorTableSGI :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr () -> m ()
glGetColorTableSGI = ffienumenumenumPtrVIOV glGetColorTableSGIFunPtr

glGetColorTableSGIFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> IO ())
glGetColorTableSGIFunPtr = unsafePerformIO (getProcAddress "glGetColorTableSGI")

{-# NOINLINE glGetColorTableSGIFunPtr #-}

pattern GL_COLOR_TABLE_ALPHA_SIZE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_ALPHA_SIZE_SGI = 0x80DD

pattern GL_COLOR_TABLE_BIAS_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_BIAS_SGI = 0x80D7

pattern GL_COLOR_TABLE_BLUE_SIZE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_BLUE_SIZE_SGI = 0x80DC

pattern GL_COLOR_TABLE_FORMAT_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_FORMAT_SGI = 0x80D8

pattern GL_COLOR_TABLE_GREEN_SIZE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_GREEN_SIZE_SGI = 0x80DB

pattern GL_COLOR_TABLE_INTENSITY_SIZE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_INTENSITY_SIZE_SGI = 0x80DF

pattern GL_COLOR_TABLE_LUMINANCE_SIZE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_LUMINANCE_SIZE_SGI = 0x80DE

pattern GL_COLOR_TABLE_RED_SIZE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_RED_SIZE_SGI = 0x80DA

pattern GL_COLOR_TABLE_SCALE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_SCALE_SGI = 0x80D6

pattern GL_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_SGI = 0x80D0

pattern GL_COLOR_TABLE_WIDTH_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_WIDTH_SGI = 0x80D9

pattern GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI = 0x80D2

pattern GL_POST_CONVOLUTION_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_COLOR_TABLE_SGI = 0x80D1

pattern GL_PROXY_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_PROXY_COLOR_TABLE_SGI = 0x80D3

pattern GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI = 0x80D5

pattern GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI = 0x80D4