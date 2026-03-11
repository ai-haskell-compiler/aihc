-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.Imaging (
  -- * Extension Support
    gl_ARB_imaging

  -- * GL_ARB_imaging
  , glBlendColor
  , glBlendEquation
  , glColorSubTable
  , glColorTable
  , glColorTableParameterfv
  , glColorTableParameteriv
  , glConvolutionFilter1D
  , glConvolutionFilter2D
  , glConvolutionParameterf
  , glConvolutionParameterfv
  , glConvolutionParameteri
  , glConvolutionParameteriv
  , glCopyColorSubTable
  , glCopyColorTable
  , glCopyConvolutionFilter1D
  , glCopyConvolutionFilter2D
  , glGetColorTable
  , glGetColorTableParameterfv
  , glGetColorTableParameteriv
  , glGetConvolutionFilter
  , glGetConvolutionParameterfv
  , glGetConvolutionParameteriv
  , glGetHistogram
  , glGetHistogramParameterfv
  , glGetHistogramParameteriv
  , glGetMinmax
  , glGetMinmaxParameterfv
  , glGetMinmaxParameteriv
  , glGetSeparableFilter
  , glHistogram
  , glMinmax
  , glResetHistogram
  , glResetMinmax
  , glSeparableFilter2D
  , pattern GL_BLEND_COLOR
  , pattern GL_BLEND_EQUATION
  , pattern GL_COLOR_MATRIX
  , pattern GL_COLOR_MATRIX_STACK_DEPTH
  , pattern GL_COLOR_TABLE
  , pattern GL_COLOR_TABLE_ALPHA_SIZE
  , pattern GL_COLOR_TABLE_BIAS
  , pattern GL_COLOR_TABLE_BLUE_SIZE
  , pattern GL_COLOR_TABLE_FORMAT
  , pattern GL_COLOR_TABLE_GREEN_SIZE
  , pattern GL_COLOR_TABLE_INTENSITY_SIZE
  , pattern GL_COLOR_TABLE_LUMINANCE_SIZE
  , pattern GL_COLOR_TABLE_RED_SIZE
  , pattern GL_COLOR_TABLE_SCALE
  , pattern GL_COLOR_TABLE_WIDTH
  , pattern GL_CONSTANT_ALPHA
  , pattern GL_CONSTANT_BORDER
  , pattern GL_CONSTANT_COLOR
  , pattern GL_CONVOLUTION_1D
  , pattern GL_CONVOLUTION_2D
  , pattern GL_CONVOLUTION_BORDER_COLOR
  , pattern GL_CONVOLUTION_BORDER_MODE
  , pattern GL_CONVOLUTION_FILTER_BIAS
  , pattern GL_CONVOLUTION_FILTER_SCALE
  , pattern GL_CONVOLUTION_FORMAT
  , pattern GL_CONVOLUTION_HEIGHT
  , pattern GL_CONVOLUTION_WIDTH
  , pattern GL_FUNC_ADD
  , pattern GL_FUNC_REVERSE_SUBTRACT
  , pattern GL_FUNC_SUBTRACT
  , pattern GL_HISTOGRAM
  , pattern GL_HISTOGRAM_ALPHA_SIZE
  , pattern GL_HISTOGRAM_BLUE_SIZE
  , pattern GL_HISTOGRAM_FORMAT
  , pattern GL_HISTOGRAM_GREEN_SIZE
  , pattern GL_HISTOGRAM_LUMINANCE_SIZE
  , pattern GL_HISTOGRAM_RED_SIZE
  , pattern GL_HISTOGRAM_SINK
  , pattern GL_HISTOGRAM_WIDTH
  , pattern GL_MAX
  , pattern GL_MAX_COLOR_MATRIX_STACK_DEPTH
  , pattern GL_MAX_CONVOLUTION_HEIGHT
  , pattern GL_MAX_CONVOLUTION_WIDTH
  , pattern GL_MIN
  , pattern GL_MINMAX
  , pattern GL_MINMAX_FORMAT
  , pattern GL_MINMAX_SINK
  , pattern GL_ONE_MINUS_CONSTANT_ALPHA
  , pattern GL_ONE_MINUS_CONSTANT_COLOR
  , pattern GL_POST_COLOR_MATRIX_ALPHA_BIAS
  , pattern GL_POST_COLOR_MATRIX_ALPHA_SCALE
  , pattern GL_POST_COLOR_MATRIX_BLUE_BIAS
  , pattern GL_POST_COLOR_MATRIX_BLUE_SCALE
  , pattern GL_POST_COLOR_MATRIX_COLOR_TABLE
  , pattern GL_POST_COLOR_MATRIX_GREEN_BIAS
  , pattern GL_POST_COLOR_MATRIX_GREEN_SCALE
  , pattern GL_POST_COLOR_MATRIX_RED_BIAS
  , pattern GL_POST_COLOR_MATRIX_RED_SCALE
  , pattern GL_POST_CONVOLUTION_ALPHA_BIAS
  , pattern GL_POST_CONVOLUTION_ALPHA_SCALE
  , pattern GL_POST_CONVOLUTION_BLUE_BIAS
  , pattern GL_POST_CONVOLUTION_BLUE_SCALE
  , pattern GL_POST_CONVOLUTION_COLOR_TABLE
  , pattern GL_POST_CONVOLUTION_GREEN_BIAS
  , pattern GL_POST_CONVOLUTION_GREEN_SCALE
  , pattern GL_POST_CONVOLUTION_RED_BIAS
  , pattern GL_POST_CONVOLUTION_RED_SCALE
  , pattern GL_PROXY_COLOR_TABLE
  , pattern GL_PROXY_HISTOGRAM
  , pattern GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE
  , pattern GL_PROXY_POST_CONVOLUTION_COLOR_TABLE
  , pattern GL_REDUCE
  , pattern GL_REPLICATE_BORDER
  , pattern GL_SEPARABLE_2D
  , pattern GL_TABLE_TOO_LARGE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_ARB_imaging extension is available.

gl_ARB_imaging :: Bool
gl_ARB_imaging = member "GL_ARB_imaging" extensions
{-# NOINLINE gl_ARB_imaging #-}

-- | Usage: @'glColorSubTable' target start count format type data@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @data@ should be @COMPSIZE(format,type,count)@.


glColorSubTable :: MonadIO m => GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glColorSubTable = ffienumsizeisizeienumenumPtrVIOV glColorSubTableFunPtr

glColorSubTableFunPtr :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glColorSubTableFunPtr = unsafePerformIO (getProcAddress "glColorSubTable")

{-# NOINLINE glColorSubTableFunPtr #-}

-- | Usage: @'glColorTable' target internalformat width format type table@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @table@ should be @COMPSIZE(format,type,width)@.


glColorTable :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glColorTable = ffienumenumsizeienumenumPtrVIOV glColorTableFunPtr

glColorTableFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glColorTableFunPtr = unsafePerformIO (getProcAddress "glColorTable")

{-# NOINLINE glColorTableFunPtr #-}

-- | Usage: @'glColorTableParameterfv' target pname params@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @pname@ is a @ColorTableParameterPName@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glColorTableParameterfv :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glColorTableParameterfv = ffienumenumPtrfloatIOV glColorTableParameterfvFunPtr

glColorTableParameterfvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glColorTableParameterfvFunPtr = unsafePerformIO (getProcAddress "glColorTableParameterfv")

{-# NOINLINE glColorTableParameterfvFunPtr #-}

-- | Usage: @'glColorTableParameteriv' target pname params@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @pname@ is a @ColorTableParameterPName@.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glColorTableParameteriv :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glColorTableParameteriv = ffienumenumPtrintIOV glColorTableParameterivFunPtr

glColorTableParameterivFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glColorTableParameterivFunPtr = unsafePerformIO (getProcAddress "glColorTableParameteriv")

{-# NOINLINE glColorTableParameterivFunPtr #-}

-- | Usage: @'glConvolutionFilter1D' target internalformat width format type image@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @image@ should be @COMPSIZE(format,type,width)@.


glConvolutionFilter1D :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glConvolutionFilter1D = ffienumenumsizeienumenumPtrVIOV glConvolutionFilter1DFunPtr

glConvolutionFilter1DFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glConvolutionFilter1DFunPtr = unsafePerformIO (getProcAddress "glConvolutionFilter1D")

{-# NOINLINE glConvolutionFilter1DFunPtr #-}

-- | Usage: @'glConvolutionFilter2D' target internalformat width height format type image@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @image@ should be @COMPSIZE(format,type,width,height)@.


glConvolutionFilter2D :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glConvolutionFilter2D = ffienumenumsizeisizeienumenumPtrVIOV glConvolutionFilter2DFunPtr

glConvolutionFilter2DFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glConvolutionFilter2DFunPtr = unsafePerformIO (getProcAddress "glConvolutionFilter2D")

{-# NOINLINE glConvolutionFilter2DFunPtr #-}

-- | Usage: @'glConvolutionParameterf' target pname params@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @pname@ is a @ConvolutionParameter@.
--
-- The parameter @params@ is a @CheckedFloat32@.


glConvolutionParameterf :: MonadIO m => GLenum -> GLenum -> GLfloat -> m ()
glConvolutionParameterf = ffienumenumfloatIOV glConvolutionParameterfFunPtr

glConvolutionParameterfFunPtr :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
glConvolutionParameterfFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameterf")

{-# NOINLINE glConvolutionParameterfFunPtr #-}

-- | Usage: @'glConvolutionParameterfv' target pname params@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @pname@ is a @ConvolutionParameter@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glConvolutionParameterfv :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glConvolutionParameterfv = ffienumenumPtrfloatIOV glConvolutionParameterfvFunPtr

glConvolutionParameterfvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glConvolutionParameterfvFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameterfv")

{-# NOINLINE glConvolutionParameterfvFunPtr #-}

-- | Usage: @'glConvolutionParameteri' target pname params@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @pname@ is a @ConvolutionParameter@.
--
-- The parameter @params@ is a @CheckedInt32@.


glConvolutionParameteri :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glConvolutionParameteri = ffienumenumintIOV glConvolutionParameteriFunPtr

glConvolutionParameteriFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glConvolutionParameteriFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameteri")

{-# NOINLINE glConvolutionParameteriFunPtr #-}

-- | Usage: @'glConvolutionParameteriv' target pname params@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @pname@ is a @ConvolutionParameter@.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glConvolutionParameteriv :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glConvolutionParameteriv = ffienumenumPtrintIOV glConvolutionParameterivFunPtr

glConvolutionParameterivFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glConvolutionParameterivFunPtr = unsafePerformIO (getProcAddress "glConvolutionParameteriv")

{-# NOINLINE glConvolutionParameterivFunPtr #-}

-- | Usage: @'glCopyColorSubTable' target start x y width@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyColorSubTable :: MonadIO m => GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> m ()
glCopyColorSubTable = ffienumsizeiintintsizeiIOV glCopyColorSubTableFunPtr

glCopyColorSubTableFunPtr :: FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())
glCopyColorSubTableFunPtr = unsafePerformIO (getProcAddress "glCopyColorSubTable")

{-# NOINLINE glCopyColorSubTableFunPtr #-}

-- | Usage: @'glCopyColorTable' target internalformat x y width@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyColorTable :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> m ()
glCopyColorTable = ffienumenumintintsizeiIOV glCopyColorTableFunPtr

glCopyColorTableFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
glCopyColorTableFunPtr = unsafePerformIO (getProcAddress "glCopyColorTable")

{-# NOINLINE glCopyColorTableFunPtr #-}

-- | Usage: @'glCopyConvolutionFilter1D' target internalformat x y width@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyConvolutionFilter1D :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> m ()
glCopyConvolutionFilter1D = ffienumenumintintsizeiIOV glCopyConvolutionFilter1DFunPtr

glCopyConvolutionFilter1DFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
glCopyConvolutionFilter1DFunPtr = unsafePerformIO (getProcAddress "glCopyConvolutionFilter1D")

{-# NOINLINE glCopyConvolutionFilter1DFunPtr #-}

-- | Usage: @'glCopyConvolutionFilter2D' target internalformat x y width height@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyConvolutionFilter2D :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyConvolutionFilter2D = ffienumenumintintsizeisizeiIOV glCopyConvolutionFilter2DFunPtr

glCopyConvolutionFilter2DFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyConvolutionFilter2DFunPtr = unsafePerformIO (getProcAddress "glCopyConvolutionFilter2D")

{-# NOINLINE glCopyConvolutionFilter2DFunPtr #-}

-- | Usage: @'glGetColorTable' target format type table@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @table@ should be @COMPSIZE(target,format,type)@.


glGetColorTable :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr () -> m ()
glGetColorTable = ffienumenumenumPtrVIOV glGetColorTableFunPtr

glGetColorTableFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> IO ())
glGetColorTableFunPtr = unsafePerformIO (getProcAddress "glGetColorTable")

{-# NOINLINE glGetColorTableFunPtr #-}

-- | Usage: @'glGetColorTableParameterfv' target pname params@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @pname@ is a @GetColorTableParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetColorTableParameterfv :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetColorTableParameterfv = ffienumenumPtrfloatIOV glGetColorTableParameterfvFunPtr

glGetColorTableParameterfvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetColorTableParameterfvFunPtr = unsafePerformIO (getProcAddress "glGetColorTableParameterfv")

{-# NOINLINE glGetColorTableParameterfvFunPtr #-}

-- | Usage: @'glGetColorTableParameteriv' target pname params@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @pname@ is a @GetColorTableParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetColorTableParameteriv :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetColorTableParameteriv = ffienumenumPtrintIOV glGetColorTableParameterivFunPtr

glGetColorTableParameterivFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetColorTableParameterivFunPtr = unsafePerformIO (getProcAddress "glGetColorTableParameteriv")

{-# NOINLINE glGetColorTableParameterivFunPtr #-}

-- | Usage: @'glGetConvolutionFilter' target format type image@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @image@ should be @COMPSIZE(target,format,type)@.


glGetConvolutionFilter :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr () -> m ()
glGetConvolutionFilter = ffienumenumenumPtrVIOV glGetConvolutionFilterFunPtr

glGetConvolutionFilterFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> IO ())
glGetConvolutionFilterFunPtr = unsafePerformIO (getProcAddress "glGetConvolutionFilter")

{-# NOINLINE glGetConvolutionFilterFunPtr #-}

-- | Usage: @'glGetConvolutionParameterfv' target pname params@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @pname@ is a @GetConvolutionParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetConvolutionParameterfv :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetConvolutionParameterfv = ffienumenumPtrfloatIOV glGetConvolutionParameterfvFunPtr

glGetConvolutionParameterfvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetConvolutionParameterfvFunPtr = unsafePerformIO (getProcAddress "glGetConvolutionParameterfv")

{-# NOINLINE glGetConvolutionParameterfvFunPtr #-}

-- | Usage: @'glGetConvolutionParameteriv' target pname params@
--
-- The parameter @target@ is a @ConvolutionTarget@.
--
-- The parameter @pname@ is a @GetConvolutionParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetConvolutionParameteriv :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetConvolutionParameteriv = ffienumenumPtrintIOV glGetConvolutionParameterivFunPtr

glGetConvolutionParameterivFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetConvolutionParameterivFunPtr = unsafePerformIO (getProcAddress "glGetConvolutionParameteriv")

{-# NOINLINE glGetConvolutionParameterivFunPtr #-}

-- | Usage: @'glGetHistogram' target reset format type values@
--
-- The parameter @target@ is a @HistogramTarget@.
--
-- The parameter @reset@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @values@ should be @COMPSIZE(target,format,type)@.


glGetHistogram :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> m ()
glGetHistogram = ffienumbooleanenumenumPtrVIOV glGetHistogramFunPtr

glGetHistogramFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> IO ())
glGetHistogramFunPtr = unsafePerformIO (getProcAddress "glGetHistogram")

{-# NOINLINE glGetHistogramFunPtr #-}

-- | Usage: @'glGetHistogramParameterfv' target pname params@
--
-- The parameter @target@ is a @HistogramTarget@.
--
-- The parameter @pname@ is a @GetHistogramParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetHistogramParameterfv :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetHistogramParameterfv = ffienumenumPtrfloatIOV glGetHistogramParameterfvFunPtr

glGetHistogramParameterfvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetHistogramParameterfvFunPtr = unsafePerformIO (getProcAddress "glGetHistogramParameterfv")

{-# NOINLINE glGetHistogramParameterfvFunPtr #-}

-- | Usage: @'glGetHistogramParameteriv' target pname params@
--
-- The parameter @target@ is a @HistogramTarget@.
--
-- The parameter @pname@ is a @GetHistogramParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetHistogramParameteriv :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetHistogramParameteriv = ffienumenumPtrintIOV glGetHistogramParameterivFunPtr

glGetHistogramParameterivFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetHistogramParameterivFunPtr = unsafePerformIO (getProcAddress "glGetHistogramParameteriv")

{-# NOINLINE glGetHistogramParameterivFunPtr #-}

-- | Usage: @'glGetMinmax' target reset format type values@
--
-- The parameter @target@ is a @MinmaxTarget@.
--
-- The parameter @reset@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @values@ should be @COMPSIZE(target,format,type)@.


glGetMinmax :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> m ()
glGetMinmax = ffienumbooleanenumenumPtrVIOV glGetMinmaxFunPtr

glGetMinmaxFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> IO ())
glGetMinmaxFunPtr = unsafePerformIO (getProcAddress "glGetMinmax")

{-# NOINLINE glGetMinmaxFunPtr #-}

-- | Usage: @'glGetMinmaxParameterfv' target pname params@
--
-- The parameter @target@ is a @MinmaxTarget@.
--
-- The parameter @pname@ is a @GetMinmaxParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMinmaxParameterfv :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetMinmaxParameterfv = ffienumenumPtrfloatIOV glGetMinmaxParameterfvFunPtr

glGetMinmaxParameterfvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetMinmaxParameterfvFunPtr = unsafePerformIO (getProcAddress "glGetMinmaxParameterfv")

{-# NOINLINE glGetMinmaxParameterfvFunPtr #-}

-- | Usage: @'glGetMinmaxParameteriv' target pname params@
--
-- The parameter @target@ is a @MinmaxTarget@.
--
-- The parameter @pname@ is a @GetMinmaxParameterPName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMinmaxParameteriv :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetMinmaxParameteriv = ffienumenumPtrintIOV glGetMinmaxParameterivFunPtr

glGetMinmaxParameterivFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetMinmaxParameterivFunPtr = unsafePerformIO (getProcAddress "glGetMinmaxParameteriv")

{-# NOINLINE glGetMinmaxParameterivFunPtr #-}

-- | Usage: @'glGetSeparableFilter' target format type row column span@
--
-- The parameter @target@ is a @SeparableTarget@.
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


glGetSeparableFilter :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> m ()
glGetSeparableFilter = ffienumenumenumPtrVPtrVPtrVIOV glGetSeparableFilterFunPtr

glGetSeparableFilterFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> IO ())
glGetSeparableFilterFunPtr = unsafePerformIO (getProcAddress "glGetSeparableFilter")

{-# NOINLINE glGetSeparableFilterFunPtr #-}

-- | Usage: @'glHistogram' target width internalformat sink@
--
-- The parameter @target@ is a @HistogramTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @sink@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glHistogram :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLboolean -> m ()
glHistogram = ffienumsizeienumbooleanIOV glHistogramFunPtr

glHistogramFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())
glHistogramFunPtr = unsafePerformIO (getProcAddress "glHistogram")

{-# NOINLINE glHistogramFunPtr #-}

-- | Usage: @'glMinmax' target internalformat sink@
--
-- The parameter @target@ is a @MinmaxTarget@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @sink@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glMinmax :: MonadIO m => GLenum -> GLenum -> GLboolean -> m ()
glMinmax = ffienumenumbooleanIOV glMinmaxFunPtr

glMinmaxFunPtr :: FunPtr (GLenum -> GLenum -> GLboolean -> IO ())
glMinmaxFunPtr = unsafePerformIO (getProcAddress "glMinmax")

{-# NOINLINE glMinmaxFunPtr #-}

-- | Usage: @'glResetHistogram' target@
--
-- The parameter @target@ is a @HistogramTarget@.


glResetHistogram :: MonadIO m => GLenum -> m ()
glResetHistogram = ffienumIOV glResetHistogramFunPtr

glResetHistogramFunPtr :: FunPtr (GLenum -> IO ())
glResetHistogramFunPtr = unsafePerformIO (getProcAddress "glResetHistogram")

{-# NOINLINE glResetHistogramFunPtr #-}

-- | Usage: @'glResetMinmax' target@
--
-- The parameter @target@ is a @MinmaxTarget@.


glResetMinmax :: MonadIO m => GLenum -> m ()
glResetMinmax = ffienumIOV glResetMinmaxFunPtr

glResetMinmaxFunPtr :: FunPtr (GLenum -> IO ())
glResetMinmaxFunPtr = unsafePerformIO (getProcAddress "glResetMinmax")

{-# NOINLINE glResetMinmaxFunPtr #-}

-- | Usage: @'glSeparableFilter2D' target internalformat width height format type row column@
--
-- The parameter @target@ is a @SeparableTarget@.
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


glSeparableFilter2D :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> m ()
glSeparableFilter2D = ffienumenumsizeisizeienumenumPtrVPtrVIOV glSeparableFilter2DFunPtr

glSeparableFilter2DFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> IO ())
glSeparableFilter2DFunPtr = unsafePerformIO (getProcAddress "glSeparableFilter2D")

{-# NOINLINE glSeparableFilter2DFunPtr #-}

pattern GL_COLOR_MATRIX  :: (Eq a, Num a) => a

pattern GL_COLOR_MATRIX = 0x80B1

pattern GL_COLOR_MATRIX_STACK_DEPTH  :: (Eq a, Num a) => a

pattern GL_COLOR_MATRIX_STACK_DEPTH = 0x80B2

pattern GL_COLOR_TABLE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE = 0x80D0

pattern GL_COLOR_TABLE_ALPHA_SIZE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_ALPHA_SIZE = 0x80DD

pattern GL_COLOR_TABLE_BIAS  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_BIAS = 0x80D7

pattern GL_COLOR_TABLE_BLUE_SIZE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_BLUE_SIZE = 0x80DC

pattern GL_COLOR_TABLE_FORMAT  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_FORMAT = 0x80D8

pattern GL_COLOR_TABLE_GREEN_SIZE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_GREEN_SIZE = 0x80DB

pattern GL_COLOR_TABLE_INTENSITY_SIZE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_INTENSITY_SIZE = 0x80DF

pattern GL_COLOR_TABLE_LUMINANCE_SIZE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_LUMINANCE_SIZE = 0x80DE

pattern GL_COLOR_TABLE_RED_SIZE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_RED_SIZE = 0x80DA

pattern GL_COLOR_TABLE_SCALE  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_SCALE = 0x80D6

pattern GL_COLOR_TABLE_WIDTH  :: (Eq a, Num a) => a

pattern GL_COLOR_TABLE_WIDTH = 0x80D9

pattern GL_CONSTANT_BORDER  :: (Eq a, Num a) => a

pattern GL_CONSTANT_BORDER = 0x8151

pattern GL_CONVOLUTION_1D  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_1D = 0x8010

pattern GL_CONVOLUTION_2D  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_2D = 0x8011

pattern GL_CONVOLUTION_BORDER_COLOR  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_BORDER_COLOR = 0x8154

pattern GL_CONVOLUTION_BORDER_MODE  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_BORDER_MODE = 0x8013

pattern GL_CONVOLUTION_FILTER_BIAS  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_FILTER_BIAS = 0x8015

pattern GL_CONVOLUTION_FILTER_SCALE  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_FILTER_SCALE = 0x8014

pattern GL_CONVOLUTION_FORMAT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_FORMAT = 0x8017

pattern GL_CONVOLUTION_HEIGHT  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_HEIGHT = 0x8019

pattern GL_CONVOLUTION_WIDTH  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_WIDTH = 0x8018

pattern GL_HISTOGRAM  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM = 0x8024

pattern GL_HISTOGRAM_ALPHA_SIZE  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_ALPHA_SIZE = 0x802B

pattern GL_HISTOGRAM_BLUE_SIZE  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_BLUE_SIZE = 0x802A

pattern GL_HISTOGRAM_FORMAT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_FORMAT = 0x8027

pattern GL_HISTOGRAM_GREEN_SIZE  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_GREEN_SIZE = 0x8029

pattern GL_HISTOGRAM_LUMINANCE_SIZE  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_LUMINANCE_SIZE = 0x802C

pattern GL_HISTOGRAM_RED_SIZE  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_RED_SIZE = 0x8028

pattern GL_HISTOGRAM_SINK  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_SINK = 0x802D

pattern GL_HISTOGRAM_WIDTH  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_WIDTH = 0x8026

pattern GL_MAX_COLOR_MATRIX_STACK_DEPTH  :: (Eq a, Num a) => a

pattern GL_MAX_COLOR_MATRIX_STACK_DEPTH = 0x80B3

pattern GL_MAX_CONVOLUTION_HEIGHT  :: (Eq a, Num a) => a

pattern GL_MAX_CONVOLUTION_HEIGHT = 0x801B

pattern GL_MAX_CONVOLUTION_WIDTH  :: (Eq a, Num a) => a

pattern GL_MAX_CONVOLUTION_WIDTH = 0x801A

pattern GL_MINMAX  :: (Eq a, Num a) => a

pattern GL_MINMAX = 0x802E

pattern GL_MINMAX_FORMAT  :: (Eq a, Num a) => a

pattern GL_MINMAX_FORMAT = 0x802F

pattern GL_MINMAX_SINK  :: (Eq a, Num a) => a

pattern GL_MINMAX_SINK = 0x8030

pattern GL_POST_COLOR_MATRIX_ALPHA_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_ALPHA_BIAS = 0x80BB

pattern GL_POST_COLOR_MATRIX_ALPHA_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_ALPHA_SCALE = 0x80B7

pattern GL_POST_COLOR_MATRIX_BLUE_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_BLUE_BIAS = 0x80BA

pattern GL_POST_COLOR_MATRIX_BLUE_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_BLUE_SCALE = 0x80B6

pattern GL_POST_COLOR_MATRIX_COLOR_TABLE  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_COLOR_TABLE = 0x80D2

pattern GL_POST_COLOR_MATRIX_GREEN_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_GREEN_BIAS = 0x80B9

pattern GL_POST_COLOR_MATRIX_GREEN_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_GREEN_SCALE = 0x80B5

pattern GL_POST_COLOR_MATRIX_RED_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_RED_BIAS = 0x80B8

pattern GL_POST_COLOR_MATRIX_RED_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_RED_SCALE = 0x80B4

pattern GL_POST_CONVOLUTION_ALPHA_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_ALPHA_BIAS = 0x8023

pattern GL_POST_CONVOLUTION_ALPHA_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_ALPHA_SCALE = 0x801F

pattern GL_POST_CONVOLUTION_BLUE_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_BLUE_BIAS = 0x8022

pattern GL_POST_CONVOLUTION_BLUE_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_BLUE_SCALE = 0x801E

pattern GL_POST_CONVOLUTION_COLOR_TABLE  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_COLOR_TABLE = 0x80D1

pattern GL_POST_CONVOLUTION_GREEN_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_GREEN_BIAS = 0x8021

pattern GL_POST_CONVOLUTION_GREEN_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_GREEN_SCALE = 0x801D

pattern GL_POST_CONVOLUTION_RED_BIAS  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_RED_BIAS = 0x8020

pattern GL_POST_CONVOLUTION_RED_SCALE  :: (Eq a, Num a) => a

pattern GL_POST_CONVOLUTION_RED_SCALE = 0x801C

pattern GL_PROXY_COLOR_TABLE  :: (Eq a, Num a) => a

pattern GL_PROXY_COLOR_TABLE = 0x80D3

pattern GL_PROXY_HISTOGRAM  :: (Eq a, Num a) => a

pattern GL_PROXY_HISTOGRAM = 0x8025

pattern GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE  :: (Eq a, Num a) => a

pattern GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = 0x80D5

pattern GL_PROXY_POST_CONVOLUTION_COLOR_TABLE  :: (Eq a, Num a) => a

pattern GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = 0x80D4

pattern GL_REDUCE  :: (Eq a, Num a) => a

pattern GL_REDUCE = 0x8016

pattern GL_REPLICATE_BORDER  :: (Eq a, Num a) => a

pattern GL_REPLICATE_BORDER = 0x8153

pattern GL_SEPARABLE_2D  :: (Eq a, Num a) => a

pattern GL_SEPARABLE_2D = 0x8012

pattern GL_TABLE_TOO_LARGE  :: (Eq a, Num a) => a

pattern GL_TABLE_TOO_LARGE = 0x8031