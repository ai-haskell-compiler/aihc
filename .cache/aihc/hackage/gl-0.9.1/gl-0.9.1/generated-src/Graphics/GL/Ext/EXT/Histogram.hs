-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Histogram (
  -- * Extension Support
    gl_EXT_histogram

  -- * GL_EXT_histogram
  , glGetHistogramEXT
  , glGetHistogramParameterfvEXT
  , glGetHistogramParameterivEXT
  , glGetMinmaxEXT
  , glGetMinmaxParameterfvEXT
  , glGetMinmaxParameterivEXT
  , glHistogramEXT
  , glMinmaxEXT
  , glResetHistogramEXT
  , glResetMinmaxEXT
  , pattern GL_HISTOGRAM_ALPHA_SIZE_EXT
  , pattern GL_HISTOGRAM_BLUE_SIZE_EXT
  , pattern GL_HISTOGRAM_EXT
  , pattern GL_HISTOGRAM_FORMAT_EXT
  , pattern GL_HISTOGRAM_GREEN_SIZE_EXT
  , pattern GL_HISTOGRAM_LUMINANCE_SIZE_EXT
  , pattern GL_HISTOGRAM_RED_SIZE_EXT
  , pattern GL_HISTOGRAM_SINK_EXT
  , pattern GL_HISTOGRAM_WIDTH_EXT
  , pattern GL_MINMAX_EXT
  , pattern GL_MINMAX_FORMAT_EXT
  , pattern GL_MINMAX_SINK_EXT
  , pattern GL_PROXY_HISTOGRAM_EXT
  , pattern GL_TABLE_TOO_LARGE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/histogram.txt GL_EXT_histogram> extension is available.

gl_EXT_histogram :: Bool
gl_EXT_histogram = member "GL_EXT_histogram" extensions
{-# NOINLINE gl_EXT_histogram #-}

-- | Usage: @'glGetHistogramEXT' target reset format type values@
--
-- The parameter @target@ is a @HistogramTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_PROXY_HISTOGRAM_EXT'.
--
-- The parameter @reset@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @values@ should be @COMPSIZE(target,format,type)@.


glGetHistogramEXT :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> m ()
glGetHistogramEXT = ffienumbooleanenumenumPtrVIOV glGetHistogramEXTFunPtr

glGetHistogramEXTFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> IO ())
glGetHistogramEXTFunPtr = unsafePerformIO (getProcAddress "glGetHistogramEXT")

{-# NOINLINE glGetHistogramEXTFunPtr #-}

-- | Usage: @'glGetHistogramParameterfvEXT' target pname params@
--
-- The parameter @target@ is a @HistogramTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_PROXY_HISTOGRAM_EXT'.
--
-- The parameter @pname@ is a @GetHistogramParameterPNameEXT@, one of: 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_ALPHA_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_BLUE_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_FORMAT_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_GREEN_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_LUMINANCE_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_RED_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_SINK_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_WIDTH_EXT'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetHistogramParameterfvEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetHistogramParameterfvEXT = ffienumenumPtrfloatIOV glGetHistogramParameterfvEXTFunPtr

glGetHistogramParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetHistogramParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetHistogramParameterfvEXT")

{-# NOINLINE glGetHistogramParameterfvEXTFunPtr #-}

-- | Usage: @'glGetHistogramParameterivEXT' target pname params@
--
-- The parameter @target@ is a @HistogramTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_PROXY_HISTOGRAM_EXT'.
--
-- The parameter @pname@ is a @GetHistogramParameterPNameEXT@, one of: 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_ALPHA_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_BLUE_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_FORMAT_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_GREEN_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_LUMINANCE_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_RED_SIZE_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_SINK_EXT', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_WIDTH_EXT'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetHistogramParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetHistogramParameterivEXT = ffienumenumPtrintIOV glGetHistogramParameterivEXTFunPtr

glGetHistogramParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetHistogramParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetHistogramParameterivEXT")

{-# NOINLINE glGetHistogramParameterivEXTFunPtr #-}

-- | Usage: @'glGetMinmaxEXT' target reset format type values@
--
-- The parameter @target@ is a @MinmaxTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT'.
--
-- The parameter @reset@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @values@ should be @COMPSIZE(target,format,type)@.


glGetMinmaxEXT :: MonadIO m => GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> m ()
glGetMinmaxEXT = ffienumbooleanenumenumPtrVIOV glGetMinmaxEXTFunPtr

glGetMinmaxEXTFunPtr :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> IO ())
glGetMinmaxEXTFunPtr = unsafePerformIO (getProcAddress "glGetMinmaxEXT")

{-# NOINLINE glGetMinmaxEXTFunPtr #-}

-- | Usage: @'glGetMinmaxParameterfvEXT' target pname params@
--
-- The parameter @target@ is a @MinmaxTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT'.
--
-- The parameter @pname@ is a @GetMinmaxParameterPNameEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX_FORMAT', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_FORMAT_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX_SINK', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_SINK_EXT'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMinmaxParameterfvEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetMinmaxParameterfvEXT = ffienumenumPtrfloatIOV glGetMinmaxParameterfvEXTFunPtr

glGetMinmaxParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetMinmaxParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetMinmaxParameterfvEXT")

{-# NOINLINE glGetMinmaxParameterfvEXTFunPtr #-}

-- | Usage: @'glGetMinmaxParameterivEXT' target pname params@
--
-- The parameter @target@ is a @MinmaxTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT'.
--
-- The parameter @pname@ is a @GetMinmaxParameterPNameEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX_FORMAT', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_FORMAT_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX_SINK', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_SINK_EXT'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMinmaxParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetMinmaxParameterivEXT = ffienumenumPtrintIOV glGetMinmaxParameterivEXTFunPtr

glGetMinmaxParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetMinmaxParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetMinmaxParameterivEXT")

{-# NOINLINE glGetMinmaxParameterivEXTFunPtr #-}

-- | Usage: @'glHistogramEXT' target width internalformat sink@
--
-- The parameter @target@ is a @HistogramTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_PROXY_HISTOGRAM_EXT'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @sink@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glHistogram'.


glHistogramEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLboolean -> m ()
glHistogramEXT = ffienumsizeienumbooleanIOV glHistogramEXTFunPtr

glHistogramEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())
glHistogramEXTFunPtr = unsafePerformIO (getProcAddress "glHistogramEXT")

{-# NOINLINE glHistogramEXTFunPtr #-}

-- | Usage: @'glMinmaxEXT' target internalformat sink@
--
-- The parameter @target@ is a @MinmaxTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT'.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.
--
-- The parameter @sink@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glMinmax'.


glMinmaxEXT :: MonadIO m => GLenum -> GLenum -> GLboolean -> m ()
glMinmaxEXT = ffienumenumbooleanIOV glMinmaxEXTFunPtr

glMinmaxEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLboolean -> IO ())
glMinmaxEXTFunPtr = unsafePerformIO (getProcAddress "glMinmaxEXT")

{-# NOINLINE glMinmaxEXTFunPtr #-}

-- | Usage: @'glResetHistogramEXT' target@
--
-- The parameter @target@ is a @HistogramTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Ext.ARB.Imaging.GL_PROXY_HISTOGRAM', 'Graphics.GL.Ext.EXT.Histogram.GL_PROXY_HISTOGRAM_EXT'.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glResetHistogram'.


glResetHistogramEXT :: MonadIO m => GLenum -> m ()
glResetHistogramEXT = ffienumIOV glResetHistogramEXTFunPtr

glResetHistogramEXTFunPtr :: FunPtr (GLenum -> IO ())
glResetHistogramEXTFunPtr = unsafePerformIO (getProcAddress "glResetHistogramEXT")

{-# NOINLINE glResetHistogramEXTFunPtr #-}

-- | Usage: @'glResetMinmaxEXT' target@
--
-- The parameter @target@ is a @MinmaxTargetEXT@, one of: 'Graphics.GL.Ext.ARB.Imaging.GL_MINMAX', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT'.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glResetMinmax'.


glResetMinmaxEXT :: MonadIO m => GLenum -> m ()
glResetMinmaxEXT = ffienumIOV glResetMinmaxEXTFunPtr

glResetMinmaxEXTFunPtr :: FunPtr (GLenum -> IO ())
glResetMinmaxEXTFunPtr = unsafePerformIO (getProcAddress "glResetMinmaxEXT")

{-# NOINLINE glResetMinmaxEXTFunPtr #-}

pattern GL_HISTOGRAM_ALPHA_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_ALPHA_SIZE_EXT = 0x802B

pattern GL_HISTOGRAM_BLUE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_BLUE_SIZE_EXT = 0x802A

pattern GL_HISTOGRAM_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_EXT = 0x8024

pattern GL_HISTOGRAM_FORMAT_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_FORMAT_EXT = 0x8027

pattern GL_HISTOGRAM_GREEN_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_GREEN_SIZE_EXT = 0x8029

pattern GL_HISTOGRAM_LUMINANCE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_LUMINANCE_SIZE_EXT = 0x802C

pattern GL_HISTOGRAM_RED_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_RED_SIZE_EXT = 0x8028

pattern GL_HISTOGRAM_SINK_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_SINK_EXT = 0x802D

pattern GL_HISTOGRAM_WIDTH_EXT  :: (Eq a, Num a) => a

pattern GL_HISTOGRAM_WIDTH_EXT = 0x8026

pattern GL_MINMAX_EXT  :: (Eq a, Num a) => a

pattern GL_MINMAX_EXT = 0x802E

pattern GL_MINMAX_FORMAT_EXT  :: (Eq a, Num a) => a

pattern GL_MINMAX_FORMAT_EXT = 0x802F

pattern GL_MINMAX_SINK_EXT  :: (Eq a, Num a) => a

pattern GL_MINMAX_SINK_EXT = 0x8030

pattern GL_PROXY_HISTOGRAM_EXT  :: (Eq a, Num a) => a

pattern GL_PROXY_HISTOGRAM_EXT = 0x8025

pattern GL_TABLE_TOO_LARGE_EXT  :: (Eq a, Num a) => a

pattern GL_TABLE_TOO_LARGE_EXT = 0x8031