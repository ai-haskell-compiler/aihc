-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Convolution (
  -- * Extension Support
    gl_EXT_convolution

  -- * GL_EXT_convolution
  , glConvolutionFilter1DEXT
  , glConvolutionFilter2DEXT
  , glConvolutionParameterfEXT
  , glConvolutionParameterfvEXT
  , glConvolutionParameteriEXT
  , glConvolutionParameterivEXT
  , glCopyConvolutionFilter1DEXT
  , glCopyConvolutionFilter2DEXT
  , glGetConvolutionFilterEXT
  , glGetConvolutionParameterfvEXT
  , glGetConvolutionParameterivEXT
  , glGetSeparableFilterEXT
  , glSeparableFilter2DEXT
  , pattern GL_CONVOLUTION_1D_EXT
  , pattern GL_CONVOLUTION_2D_EXT
  , pattern GL_CONVOLUTION_BORDER_MODE_EXT
  , pattern GL_CONVOLUTION_FILTER_BIAS_EXT
  , pattern GL_CONVOLUTION_FILTER_SCALE_EXT
  , pattern GL_CONVOLUTION_FORMAT_EXT
  , pattern GL_CONVOLUTION_HEIGHT_EXT
  , pattern GL_CONVOLUTION_WIDTH_EXT
  , pattern GL_MAX_CONVOLUTION_HEIGHT_EXT
  , pattern GL_MAX_CONVOLUTION_WIDTH_EXT
  , pattern GL_POST_CONVOLUTION_ALPHA_BIAS_EXT
  , pattern GL_POST_CONVOLUTION_ALPHA_SCALE_EXT
  , pattern GL_POST_CONVOLUTION_BLUE_BIAS_EXT
  , pattern GL_POST_CONVOLUTION_BLUE_SCALE_EXT
  , pattern GL_POST_CONVOLUTION_GREEN_BIAS_EXT
  , pattern GL_POST_CONVOLUTION_GREEN_SCALE_EXT
  , pattern GL_POST_CONVOLUTION_RED_BIAS_EXT
  , pattern GL_POST_CONVOLUTION_RED_SCALE_EXT
  , pattern GL_REDUCE_EXT
  , pattern GL_SEPARABLE_2D_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/convolution.txt GL_EXT_convolution> extension is available.

gl_EXT_convolution :: Bool
gl_EXT_convolution = member "GL_EXT_convolution" extensions
{-# NOINLINE gl_EXT_convolution #-}

-- | Usage: @'glConvolutionFilter1DEXT' target internalformat width format type image@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @image@ should be @COMPSIZE(format,type,width)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glConvolutionFilter1D'.


glConvolutionFilter1DEXT :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glConvolutionFilter1DEXT = ffienumenumsizeienumenumPtrVIOV glConvolutionFilter1DEXTFunPtr

glConvolutionFilter1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glConvolutionFilter1DEXTFunPtr = unsafePerformIO (getProcAddress "glConvolutionFilter1DEXT")

{-# NOINLINE glConvolutionFilter1DEXTFunPtr #-}

-- | Usage: @'glConvolutionFilter2DEXT' target internalformat width height format type image@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @image@ should be @COMPSIZE(format,type,width,height)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glConvolutionFilter2D'.


glConvolutionFilter2DEXT :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glConvolutionFilter2DEXT = ffienumenumsizeisizeienumenumPtrVIOV glConvolutionFilter2DEXTFunPtr

glConvolutionFilter2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glConvolutionFilter2DEXTFunPtr = unsafePerformIO (getProcAddress "glConvolutionFilter2DEXT")

{-# NOINLINE glConvolutionFilter2DEXTFunPtr #-}

-- | Usage: @'glConvolutionParameterfEXT' target pname params@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @pname@ is a @ConvolutionParameterEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_BORDER_MODE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_BORDER_MODE_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_BIAS', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_BIAS_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_SCALE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_SCALE_EXT'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glConvolutionParameterf'.


glConvolutionParameterfEXT :: MonadIO m => GLenum -> GLenum -> GLfloat -> m ()
glConvolutionParameterfEXT = ffienumenumfloatIOV glConvolutionParameterfEXTFunPtr

glConvolutionParameterfEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
glConvolutionParameterfEXTFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameterfEXT")

{-# NOINLINE glConvolutionParameterfEXTFunPtr #-}

-- | Usage: @'glConvolutionParameterfvEXT' target pname params@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @pname@ is a @ConvolutionParameterEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_BORDER_MODE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_BORDER_MODE_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_BIAS', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_BIAS_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_SCALE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_SCALE_EXT'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glConvolutionParameterfv'.


glConvolutionParameterfvEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glConvolutionParameterfvEXT = ffienumenumPtrfloatIOV glConvolutionParameterfvEXTFunPtr

glConvolutionParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glConvolutionParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameterfvEXT")

{-# NOINLINE glConvolutionParameterfvEXTFunPtr #-}

-- | Usage: @'glConvolutionParameteriEXT' target pname params@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @pname@ is a @ConvolutionParameterEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_BORDER_MODE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_BORDER_MODE_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_BIAS', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_BIAS_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_SCALE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_SCALE_EXT'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glConvolutionParameteri'.


glConvolutionParameteriEXT :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glConvolutionParameteriEXT = ffienumenumintIOV glConvolutionParameteriEXTFunPtr

glConvolutionParameteriEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glConvolutionParameteriEXTFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameteriEXT")

{-# NOINLINE glConvolutionParameteriEXTFunPtr #-}

-- | Usage: @'glConvolutionParameterivEXT' target pname params@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @pname@ is a @ConvolutionParameterEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_BORDER_MODE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_BORDER_MODE_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_BIAS', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_BIAS_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_SCALE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_SCALE_EXT'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glConvolutionParameteriv'.


glConvolutionParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glConvolutionParameterivEXT = ffienumenumPtrintIOV glConvolutionParameterivEXTFunPtr

glConvolutionParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glConvolutionParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameterivEXT")

{-# NOINLINE glConvolutionParameterivEXTFunPtr #-}

-- | Usage: @'glCopyConvolutionFilter1DEXT' target internalformat x y width@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glCopyConvolutionFilter1D'.


glCopyConvolutionFilter1DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> m ()
glCopyConvolutionFilter1DEXT = ffienumenumintintsizeiIOV glCopyConvolutionFilter1DEXTFunPtr

glCopyConvolutionFilter1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
glCopyConvolutionFilter1DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyConvolutionFilter1DEXT")

{-# NOINLINE glCopyConvolutionFilter1DEXTFunPtr #-}

-- | Usage: @'glCopyConvolutionFilter2DEXT' target internalformat x y width height@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glCopyConvolutionFilter2D'.


glCopyConvolutionFilter2DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyConvolutionFilter2DEXT = ffienumenumintintsizeisizeiIOV glCopyConvolutionFilter2DEXTFunPtr

glCopyConvolutionFilter2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyConvolutionFilter2DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyConvolutionFilter2DEXT")

{-# NOINLINE glCopyConvolutionFilter2DEXTFunPtr #-}

-- | Usage: @'glGetConvolutionFilterEXT' target format type image@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @image@ should be @COMPSIZE(target,format,type)@.


glGetConvolutionFilterEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr () -> m ()
glGetConvolutionFilterEXT = ffienumenumenumPtrVIOV glGetConvolutionFilterEXTFunPtr

glGetConvolutionFilterEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> IO ())
glGetConvolutionFilterEXTFunPtr = unsafePerformIO (getProcAddress "glGetConvolutionFilterEXT")

{-# NOINLINE glGetConvolutionFilterEXTFunPtr #-}

-- | Usage: @'glGetConvolutionParameterfvEXT' target pname params@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @pname@ is a @ConvolutionParameterEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_BORDER_MODE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_BORDER_MODE_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_BIAS', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_BIAS_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_SCALE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_SCALE_EXT'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetConvolutionParameterfvEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetConvolutionParameterfvEXT = ffienumenumPtrfloatIOV glGetConvolutionParameterfvEXTFunPtr

glGetConvolutionParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetConvolutionParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetConvolutionParameterfvEXT")

{-# NOINLINE glGetConvolutionParameterfvEXTFunPtr #-}

-- | Usage: @'glGetConvolutionParameterivEXT' target pname params@
--
-- The parameter @target@ is a @ConvolutionTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_1D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT'.
--
-- The parameter @pname@ is a @ConvolutionParameterEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_BORDER_MODE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_BORDER_MODE_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_BIAS', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_BIAS_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_CONVOLUTION_FILTER_SCALE', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_FILTER_SCALE_EXT'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetConvolutionParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetConvolutionParameterivEXT = ffienumenumPtrintIOV glGetConvolutionParameterivEXTFunPtr

glGetConvolutionParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetConvolutionParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetConvolutionParameterivEXT")

{-# NOINLINE glGetConvolutionParameterivEXTFunPtr #-}

-- | Usage: @'glGetSeparableFilterEXT' target format type row column span@
--
-- The parameter @target@ is a @SeparableTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_SEPARABLE_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT'.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @row@ should be @COMPSIZE(target,format,type)@.
--
-- The length of @column@ should be @COMPSIZE(target,format,type)@.
--
-- The length of @span@ should be @COMPSIZE(target,format,type)@.


glGetSeparableFilterEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> m ()
glGetSeparableFilterEXT = ffienumenumenumPtrVPtrVPtrVIOV glGetSeparableFilterEXTFunPtr

glGetSeparableFilterEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> IO ())
glGetSeparableFilterEXTFunPtr = unsafePerformIO (getProcAddress "glGetSeparableFilterEXT")

{-# NOINLINE glGetSeparableFilterEXTFunPtr #-}

-- | Usage: @'glSeparableFilter2DEXT' target internalformat width height format type row column@
--
-- The parameter @target@ is a @SeparableTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_SEPARABLE_2D', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @row@ should be @COMPSIZE(target,format,type,width)@.
--
-- The length of @column@ should be @COMPSIZE(target,format,type,height)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glSeparableFilter2D'.


glSeparableFilter2DEXT :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> m ()
glSeparableFilter2DEXT = ffienumenumsizeisizeienumenumPtrVPtrVIOV glSeparableFilter2DEXTFunPtr

glSeparableFilter2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> IO ())
glSeparableFilter2DEXTFunPtr = unsafePerformIO (getProcAddress "glSeparableFilter2DEXT")

{-# NOINLINE glSeparableFilter2DEXTFunPtr #-}

pattern GL_CONVOLUTION_1D_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_1D_EXT = 0x8010

pattern GL_CONVOLUTION_2D_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_2D_EXT = 0x8011

pattern GL_CONVOLUTION_BORDER_MODE_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_BORDER_MODE_EXT = 0x8013

pattern GL_CONVOLUTION_FILTER_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_FILTER_BIAS_EXT = 0x8015

pattern GL_CONVOLUTION_FILTER_SCALE_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_FILTER_SCALE_EXT = 0x8014

pattern GL_CONVOLUTION_FORMAT_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_FORMAT_EXT = 0x8017

pattern GL_CONVOLUTION_HEIGHT_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_HEIGHT_EXT = 0x8019

pattern GL_CONVOLUTION_WIDTH_EXT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_WIDTH_EXT = 0x8018

pattern GL_MAX_CONVOLUTION_HEIGHT_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_CONVOLUTION_HEIGHT_EXT = 0x801B

pattern GL_MAX_CONVOLUTION_WIDTH_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_CONVOLUTION_WIDTH_EXT = 0x801A

pattern GL_POST_CONVOLUTION_ALPHA_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_ALPHA_BIAS_EXT = 0x8023

pattern GL_POST_CONVOLUTION_ALPHA_SCALE_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_ALPHA_SCALE_EXT = 0x801F

pattern GL_POST_CONVOLUTION_BLUE_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_BLUE_BIAS_EXT = 0x8022

pattern GL_POST_CONVOLUTION_BLUE_SCALE_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_BLUE_SCALE_EXT = 0x801E

pattern GL_POST_CONVOLUTION_GREEN_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_GREEN_BIAS_EXT = 0x8021

pattern GL_POST_CONVOLUTION_GREEN_SCALE_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_GREEN_SCALE_EXT = 0x801D

pattern GL_POST_CONVOLUTION_RED_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_RED_BIAS_EXT = 0x8020

pattern GL_POST_CONVOLUTION_RED_SCALE_EXT  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_RED_SCALE_EXT = 0x801C

pattern GL_REDUCE_EXT  :: (Eq a, Num a) => a

pattern GL_REDUCE_EXT = 0x8016

pattern GL_SEPARABLE_2D_EXT  :: (Eq a, Num a) => a

pattern GL_SEPARABLE_2D_EXT = 0x8012