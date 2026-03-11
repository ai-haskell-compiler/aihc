-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.VertexArrayObject (
  -- * Extension Support
    gl_ATI_vertex_array_object

  -- * GL_ATI_vertex_array_object
  , glArrayObjectATI
  , glFreeObjectBufferATI
  , glGetArrayObjectfvATI
  , glGetArrayObjectivATI
  , glGetObjectBufferfvATI
  , glGetObjectBufferivATI
  , glGetVariantArrayObjectfvATI
  , glGetVariantArrayObjectivATI
  , glIsObjectBufferATI
  , glNewObjectBufferATI
  , glUpdateObjectBufferATI
  , glVariantArrayObjectATI
  , pattern GL_ARRAY_OBJECT_BUFFER_ATI
  , pattern GL_ARRAY_OBJECT_OFFSET_ATI
  , pattern GL_DISCARD_ATI
  , pattern GL_DYNAMIC_ATI
  , pattern GL_OBJECT_BUFFER_SIZE_ATI
  , pattern GL_OBJECT_BUFFER_USAGE_ATI
  , pattern GL_PRESERVE_ATI
  , pattern GL_STATIC_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/vertex_array_object.txt GL_ATI_vertex_array_object> extension is available.

gl_ATI_vertex_array_object :: Bool
gl_ATI_vertex_array_object = member "GL_ATI_vertex_array_object" extensions
{-# NOINLINE gl_ATI_vertex_array_object #-}

-- | Usage: @'glArrayObjectATI' array size type stride buffer offset@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.
--
-- The parameter @type@ is a @ScalarType@.


glArrayObjectATI :: MonadIO m => GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> m ()
glArrayObjectATI = ffienumintenumsizeiuintuintIOV glArrayObjectATIFunPtr

glArrayObjectATIFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ())
glArrayObjectATIFunPtr = unsafePerformIO (getProcAddress "glArrayObjectATI")

{-# NOINLINE glArrayObjectATIFunPtr #-}

-- | Usage: @'glFreeObjectBufferATI' buffer@


glFreeObjectBufferATI :: MonadIO m => GLuint -> m ()
glFreeObjectBufferATI = ffiuintIOV glFreeObjectBufferATIFunPtr

glFreeObjectBufferATIFunPtr :: FunPtr (GLuint -> IO ())
glFreeObjectBufferATIFunPtr = unsafePerformIO (getProcAddress "glFreeObjectBufferATI")

{-# NOINLINE glFreeObjectBufferATIFunPtr #-}

-- | Usage: @'glGetArrayObjectfvATI' array pname params@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @1@.


glGetArrayObjectfvATI :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetArrayObjectfvATI = ffienumenumPtrfloatIOV glGetArrayObjectfvATIFunPtr

glGetArrayObjectfvATIFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetArrayObjectfvATIFunPtr = unsafePerformIO (getProcAddress "glGetArrayObjectfvATI")

{-# NOINLINE glGetArrayObjectfvATIFunPtr #-}

-- | Usage: @'glGetArrayObjectivATI' array pname params@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @1@.


glGetArrayObjectivATI :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetArrayObjectivATI = ffienumenumPtrintIOV glGetArrayObjectivATIFunPtr

glGetArrayObjectivATIFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetArrayObjectivATIFunPtr = unsafePerformIO (getProcAddress "glGetArrayObjectivATI")

{-# NOINLINE glGetArrayObjectivATIFunPtr #-}

-- | Usage: @'glGetObjectBufferfvATI' buffer pname params@
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @1@.


glGetObjectBufferfvATI :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetObjectBufferfvATI = ffiuintenumPtrfloatIOV glGetObjectBufferfvATIFunPtr

glGetObjectBufferfvATIFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetObjectBufferfvATIFunPtr = unsafePerformIO (getProcAddress "glGetObjectBufferfvATI")

{-# NOINLINE glGetObjectBufferfvATIFunPtr #-}

-- | Usage: @'glGetObjectBufferivATI' buffer pname params@
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @1@.


glGetObjectBufferivATI :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetObjectBufferivATI = ffiuintenumPtrintIOV glGetObjectBufferivATIFunPtr

glGetObjectBufferivATIFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetObjectBufferivATIFunPtr = unsafePerformIO (getProcAddress "glGetObjectBufferivATI")

{-# NOINLINE glGetObjectBufferivATIFunPtr #-}

-- | Usage: @'glGetVariantArrayObjectfvATI' id pname params@
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @1@.


glGetVariantArrayObjectfvATI :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetVariantArrayObjectfvATI = ffiuintenumPtrfloatIOV glGetVariantArrayObjectfvATIFunPtr

glGetVariantArrayObjectfvATIFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetVariantArrayObjectfvATIFunPtr = unsafePerformIO (getProcAddress "glGetVariantArrayObjectfvATI")

{-# NOINLINE glGetVariantArrayObjectfvATIFunPtr #-}

-- | Usage: @'glGetVariantArrayObjectivATI' id pname params@
--
-- The parameter @pname@ is a @ArrayObjectPNameATI@.
--
-- The length of @params@ should be @1@.


glGetVariantArrayObjectivATI :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVariantArrayObjectivATI = ffiuintenumPtrintIOV glGetVariantArrayObjectivATIFunPtr

glGetVariantArrayObjectivATIFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVariantArrayObjectivATIFunPtr = unsafePerformIO (getProcAddress "glGetVariantArrayObjectivATI")

{-# NOINLINE glGetVariantArrayObjectivATIFunPtr #-}

-- | Usage: @'glIsObjectBufferATI' buffer@


glIsObjectBufferATI :: MonadIO m => GLuint -> m GLboolean
glIsObjectBufferATI = ffiuintIOboolean glIsObjectBufferATIFunPtr

glIsObjectBufferATIFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsObjectBufferATIFunPtr = unsafePerformIO (getProcAddress "glIsObjectBufferATI")

{-# NOINLINE glIsObjectBufferATIFunPtr #-}

-- | Usage: @'glNewObjectBufferATI' size pointer usage@
--
-- The parameter @usage@ is a @ArrayObjectUsageATI@.
--
-- The length of @pointer@ should be @size@.


glNewObjectBufferATI :: MonadIO m => GLsizei -> Ptr () -> GLenum -> m GLuint
glNewObjectBufferATI = ffisizeiPtrVenumIOuint glNewObjectBufferATIFunPtr

glNewObjectBufferATIFunPtr :: FunPtr (GLsizei -> Ptr () -> GLenum -> IO GLuint)
glNewObjectBufferATIFunPtr = unsafePerformIO (getProcAddress "glNewObjectBufferATI")

{-# NOINLINE glNewObjectBufferATIFunPtr #-}

-- | Usage: @'glUpdateObjectBufferATI' buffer offset size pointer preserve@
--
-- The parameter @preserve@ is a @PreserveModeATI@.
--
-- The length of @pointer@ should be @size@.


glUpdateObjectBufferATI :: MonadIO m => GLuint -> GLuint -> GLsizei -> Ptr () -> GLenum -> m ()
glUpdateObjectBufferATI = ffiuintuintsizeiPtrVenumIOV glUpdateObjectBufferATIFunPtr

glUpdateObjectBufferATIFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr () -> GLenum -> IO ())
glUpdateObjectBufferATIFunPtr = unsafePerformIO (getProcAddress "glUpdateObjectBufferATI")

{-# NOINLINE glUpdateObjectBufferATIFunPtr #-}

-- | Usage: @'glVariantArrayObjectATI' id type stride buffer offset@
--
-- The parameter @type@ is a @ScalarType@.


glVariantArrayObjectATI :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> m ()
glVariantArrayObjectATI = ffiuintenumsizeiuintuintIOV glVariantArrayObjectATIFunPtr

glVariantArrayObjectATIFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ())
glVariantArrayObjectATIFunPtr = unsafePerformIO (getProcAddress "glVariantArrayObjectATI")

{-# NOINLINE glVariantArrayObjectATIFunPtr #-}

pattern GL_ARRAY_OBJECT_BUFFER_ATI  :: (Eq a, Num a) => a

pattern GL_ARRAY_OBJECT_BUFFER_ATI = 0x8766

pattern GL_ARRAY_OBJECT_OFFSET_ATI  :: (Eq a, Num a) => a

pattern GL_ARRAY_OBJECT_OFFSET_ATI = 0x8767

pattern GL_DISCARD_ATI  :: (Eq a, Num a) => a

pattern GL_DISCARD_ATI = 0x8763

pattern GL_DYNAMIC_ATI  :: (Eq a, Num a) => a

pattern GL_DYNAMIC_ATI = 0x8761

pattern GL_OBJECT_BUFFER_SIZE_ATI  :: (Eq a, Num a) => a

pattern GL_OBJECT_BUFFER_SIZE_ATI = 0x8764

pattern GL_OBJECT_BUFFER_USAGE_ATI  :: (Eq a, Num a) => a

pattern GL_OBJECT_BUFFER_USAGE_ATI = 0x8765

pattern GL_PRESERVE_ATI  :: (Eq a, Num a) => a

pattern GL_PRESERVE_ATI = 0x8762

pattern GL_STATIC_ATI  :: (Eq a, Num a) => a

pattern GL_STATIC_ATI = 0x8760