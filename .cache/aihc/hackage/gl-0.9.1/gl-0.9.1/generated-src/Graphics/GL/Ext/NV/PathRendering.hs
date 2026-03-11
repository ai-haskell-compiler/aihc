-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PathRendering (
  -- * Extension Support
    gl_NV_path_rendering

  -- * GL_NV_path_rendering
  , glCopyPathNV
  , glCoverFillPathInstancedNV
  , glCoverFillPathNV
  , glCoverStrokePathInstancedNV
  , glCoverStrokePathNV
  , glDeletePathsNV
  , glGenPathsNV
  , glGetPathColorGenfvNV
  , glGetPathColorGenivNV
  , glGetPathCommandsNV
  , glGetPathCoordsNV
  , glGetPathDashArrayNV
  , glGetPathLengthNV
  , glGetPathMetricRangeNV
  , glGetPathMetricsNV
  , glGetPathParameterfvNV
  , glGetPathParameterivNV
  , glGetPathSpacingNV
  , glGetPathTexGenfvNV
  , glGetPathTexGenivNV
  , glGetProgramResourcefvNV
  , glInterpolatePathsNV
  , glIsPathNV
  , glIsPointInFillPathNV
  , glIsPointInStrokePathNV
  , glMatrixLoad3x2fNV
  , glMatrixLoad3x3fNV
  , glMatrixLoadTranspose3x3fNV
  , glMatrixMult3x2fNV
  , glMatrixMult3x3fNV
  , glMatrixMultTranspose3x3fNV
  , glPathColorGenNV
  , glPathCommandsNV
  , glPathCoordsNV
  , glPathCoverDepthFuncNV
  , glPathDashArrayNV
  , glPathFogGenNV
  , glPathGlyphIndexArrayNV
  , glPathGlyphIndexRangeNV
  , glPathGlyphRangeNV
  , glPathGlyphsNV
  , glPathMemoryGlyphIndexArrayNV
  , glPathParameterfNV
  , glPathParameterfvNV
  , glPathParameteriNV
  , glPathParameterivNV
  , glPathStencilDepthOffsetNV
  , glPathStencilFuncNV
  , glPathStringNV
  , glPathSubCommandsNV
  , glPathSubCoordsNV
  , glPathTexGenNV
  , glPointAlongPathNV
  , glProgramPathFragmentInputGenNV
  , glStencilFillPathInstancedNV
  , glStencilFillPathNV
  , glStencilStrokePathInstancedNV
  , glStencilStrokePathNV
  , glStencilThenCoverFillPathInstancedNV
  , glStencilThenCoverFillPathNV
  , glStencilThenCoverStrokePathInstancedNV
  , glStencilThenCoverStrokePathNV
  , glTransformPathNV
  , glWeightPathsNV
  , pattern GL_2_BYTES_NV
  , pattern GL_3_BYTES_NV
  , pattern GL_4_BYTES_NV
  , pattern GL_ACCUM_ADJACENT_PAIRS_NV
  , pattern GL_ADJACENT_PAIRS_NV
  , pattern GL_AFFINE_2D_NV
  , pattern GL_AFFINE_3D_NV
  , pattern GL_ARC_TO_NV
  , pattern GL_BEVEL_NV
  , pattern GL_BOLD_BIT_NV
  , pattern GL_BOUNDING_BOX_NV
  , pattern GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV
  , pattern GL_CIRCULAR_CCW_ARC_TO_NV
  , pattern GL_CIRCULAR_CW_ARC_TO_NV
  , pattern GL_CIRCULAR_TANGENT_ARC_TO_NV
  , pattern GL_CLOSE_PATH_NV
  , pattern GL_CONIC_CURVE_TO_NV
  , pattern GL_CONSTANT_NV
  , pattern GL_CONVEX_HULL_NV
  , pattern GL_COUNT_DOWN_NV
  , pattern GL_COUNT_UP_NV
  , pattern GL_CUBIC_CURVE_TO_NV
  , pattern GL_DUP_FIRST_CUBIC_CURVE_TO_NV
  , pattern GL_DUP_LAST_CUBIC_CURVE_TO_NV
  , pattern GL_EYE_LINEAR_NV
  , pattern GL_FILE_NAME_NV
  , pattern GL_FIRST_TO_REST_NV
  , pattern GL_FONT_ASCENDER_BIT_NV
  , pattern GL_FONT_DESCENDER_BIT_NV
  , pattern GL_FONT_GLYPHS_AVAILABLE_NV
  , pattern GL_FONT_HAS_KERNING_BIT_NV
  , pattern GL_FONT_HEIGHT_BIT_NV
  , pattern GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV
  , pattern GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV
  , pattern GL_FONT_NUM_GLYPH_INDICES_BIT_NV
  , pattern GL_FONT_TARGET_UNAVAILABLE_NV
  , pattern GL_FONT_UNAVAILABLE_NV
  , pattern GL_FONT_UNDERLINE_POSITION_BIT_NV
  , pattern GL_FONT_UNDERLINE_THICKNESS_BIT_NV
  , pattern GL_FONT_UNINTELLIGIBLE_NV
  , pattern GL_FONT_UNITS_PER_EM_BIT_NV
  , pattern GL_FONT_X_MAX_BOUNDS_BIT_NV
  , pattern GL_FONT_X_MIN_BOUNDS_BIT_NV
  , pattern GL_FONT_Y_MAX_BOUNDS_BIT_NV
  , pattern GL_FONT_Y_MIN_BOUNDS_BIT_NV
  , pattern GL_FRAGMENT_INPUT_NV
  , pattern GL_GLYPH_HAS_KERNING_BIT_NV
  , pattern GL_GLYPH_HEIGHT_BIT_NV
  , pattern GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV
  , pattern GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV
  , pattern GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV
  , pattern GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV
  , pattern GL_GLYPH_VERTICAL_BEARING_X_BIT_NV
  , pattern GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV
  , pattern GL_GLYPH_WIDTH_BIT_NV
  , pattern GL_HORIZONTAL_LINE_TO_NV
  , pattern GL_ITALIC_BIT_NV
  , pattern GL_LARGE_CCW_ARC_TO_NV
  , pattern GL_LARGE_CW_ARC_TO_NV
  , pattern GL_LINE_TO_NV
  , pattern GL_MITER_REVERT_NV
  , pattern GL_MITER_TRUNCATE_NV
  , pattern GL_MOVE_TO_CONTINUES_NV
  , pattern GL_MOVE_TO_NV
  , pattern GL_MOVE_TO_RESETS_NV
  , pattern GL_OBJECT_LINEAR_NV
  , pattern GL_PATH_CLIENT_LENGTH_NV
  , pattern GL_PATH_COMMAND_COUNT_NV
  , pattern GL_PATH_COMPUTED_LENGTH_NV
  , pattern GL_PATH_COORD_COUNT_NV
  , pattern GL_PATH_COVER_DEPTH_FUNC_NV
  , pattern GL_PATH_DASH_ARRAY_COUNT_NV
  , pattern GL_PATH_DASH_CAPS_NV
  , pattern GL_PATH_DASH_OFFSET_NV
  , pattern GL_PATH_DASH_OFFSET_RESET_NV
  , pattern GL_PATH_END_CAPS_NV
  , pattern GL_PATH_ERROR_POSITION_NV
  , pattern GL_PATH_FILL_BOUNDING_BOX_NV
  , pattern GL_PATH_FILL_COVER_MODE_NV
  , pattern GL_PATH_FILL_MASK_NV
  , pattern GL_PATH_FILL_MODE_NV
  , pattern GL_PATH_FOG_GEN_MODE_NV
  , pattern GL_PATH_FORMAT_PS_NV
  , pattern GL_PATH_FORMAT_SVG_NV
  , pattern GL_PATH_GEN_COEFF_NV
  , pattern GL_PATH_GEN_COLOR_FORMAT_NV
  , pattern GL_PATH_GEN_COMPONENTS_NV
  , pattern GL_PATH_GEN_MODE_NV
  , pattern GL_PATH_INITIAL_DASH_CAP_NV
  , pattern GL_PATH_INITIAL_END_CAP_NV
  , pattern GL_PATH_JOIN_STYLE_NV
  , pattern GL_PATH_MAX_MODELVIEW_STACK_DEPTH_NV
  , pattern GL_PATH_MAX_PROJECTION_STACK_DEPTH_NV
  , pattern GL_PATH_MITER_LIMIT_NV
  , pattern GL_PATH_MODELVIEW_MATRIX_NV
  , pattern GL_PATH_MODELVIEW_NV
  , pattern GL_PATH_MODELVIEW_STACK_DEPTH_NV
  , pattern GL_PATH_OBJECT_BOUNDING_BOX_NV
  , pattern GL_PATH_PROJECTION_MATRIX_NV
  , pattern GL_PATH_PROJECTION_NV
  , pattern GL_PATH_PROJECTION_STACK_DEPTH_NV
  , pattern GL_PATH_STENCIL_DEPTH_OFFSET_FACTOR_NV
  , pattern GL_PATH_STENCIL_DEPTH_OFFSET_UNITS_NV
  , pattern GL_PATH_STENCIL_FUNC_NV
  , pattern GL_PATH_STENCIL_REF_NV
  , pattern GL_PATH_STENCIL_VALUE_MASK_NV
  , pattern GL_PATH_STROKE_BOUNDING_BOX_NV
  , pattern GL_PATH_STROKE_COVER_MODE_NV
  , pattern GL_PATH_STROKE_MASK_NV
  , pattern GL_PATH_STROKE_WIDTH_NV
  , pattern GL_PATH_TERMINAL_DASH_CAP_NV
  , pattern GL_PATH_TERMINAL_END_CAP_NV
  , pattern GL_PATH_TRANSPOSE_MODELVIEW_MATRIX_NV
  , pattern GL_PATH_TRANSPOSE_PROJECTION_MATRIX_NV
  , pattern GL_PRIMARY_COLOR
  , pattern GL_PRIMARY_COLOR_NV
  , pattern GL_QUADRATIC_CURVE_TO_NV
  , pattern GL_RECT_NV
  , pattern GL_RELATIVE_ARC_TO_NV
  , pattern GL_RELATIVE_CONIC_CURVE_TO_NV
  , pattern GL_RELATIVE_CUBIC_CURVE_TO_NV
  , pattern GL_RELATIVE_HORIZONTAL_LINE_TO_NV
  , pattern GL_RELATIVE_LARGE_CCW_ARC_TO_NV
  , pattern GL_RELATIVE_LARGE_CW_ARC_TO_NV
  , pattern GL_RELATIVE_LINE_TO_NV
  , pattern GL_RELATIVE_MOVE_TO_NV
  , pattern GL_RELATIVE_QUADRATIC_CURVE_TO_NV
  , pattern GL_RELATIVE_RECT_NV
  , pattern GL_RELATIVE_ROUNDED_RECT2_NV
  , pattern GL_RELATIVE_ROUNDED_RECT4_NV
  , pattern GL_RELATIVE_ROUNDED_RECT8_NV
  , pattern GL_RELATIVE_ROUNDED_RECT_NV
  , pattern GL_RELATIVE_SMALL_CCW_ARC_TO_NV
  , pattern GL_RELATIVE_SMALL_CW_ARC_TO_NV
  , pattern GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV
  , pattern GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV
  , pattern GL_RELATIVE_VERTICAL_LINE_TO_NV
  , pattern GL_RESTART_PATH_NV
  , pattern GL_ROUNDED_RECT2_NV
  , pattern GL_ROUNDED_RECT4_NV
  , pattern GL_ROUNDED_RECT8_NV
  , pattern GL_ROUNDED_RECT_NV
  , pattern GL_ROUND_NV
  , pattern GL_SECONDARY_COLOR_NV
  , pattern GL_SKIP_MISSING_GLYPH_NV
  , pattern GL_SMALL_CCW_ARC_TO_NV
  , pattern GL_SMALL_CW_ARC_TO_NV
  , pattern GL_SMOOTH_CUBIC_CURVE_TO_NV
  , pattern GL_SMOOTH_QUADRATIC_CURVE_TO_NV
  , pattern GL_SQUARE_NV
  , pattern GL_STANDARD_FONT_FORMAT_NV
  , pattern GL_STANDARD_FONT_NAME_NV
  , pattern GL_SYSTEM_FONT_NAME_NV
  , pattern GL_TRANSLATE_2D_NV
  , pattern GL_TRANSLATE_3D_NV
  , pattern GL_TRANSLATE_X_NV
  , pattern GL_TRANSLATE_Y_NV
  , pattern GL_TRANSPOSE_AFFINE_2D_NV
  , pattern GL_TRANSPOSE_AFFINE_3D_NV
  , pattern GL_TRIANGULAR_NV
  , pattern GL_USE_MISSING_GLYPH_NV
  , pattern GL_UTF16_NV
  , pattern GL_UTF8_NV
  , pattern GL_VERTICAL_LINE_TO_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/path_rendering.txt GL_NV_path_rendering> extension is available.

gl_NV_path_rendering :: Bool
gl_NV_path_rendering = member "GL_NV_path_rendering" extensions
{-# NOINLINE gl_NV_path_rendering #-}

-- | Usage: @'glCopyPathNV' resultPath srcPath@
--
-- The parameter @resultPath@ is a @Path@.
--
-- The parameter @srcPath@ is a @Path@.


glCopyPathNV :: MonadIO m => GLuint -> GLuint -> m ()
glCopyPathNV = ffiuintuintIOV glCopyPathNVFunPtr

glCopyPathNVFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glCopyPathNVFunPtr = unsafePerformIO (getProcAddress "glCopyPathNV")

{-# NOINLINE glCopyPathNVFunPtr #-}

-- | Usage: @'glCoverFillPathInstancedNV' numPaths pathNameType paths pathBase coverMode transformType transformValues@
--
-- The parameter @pathNameType@ is a @PathElementType@.
--
-- The parameter @paths@ is a @PathElement@.
--
-- The parameter @pathBase@ is a @Path@.
--
-- The parameter @coverMode@ is a @PathCoverMode@.
--
-- The parameter @transformType@ is a @PathTransformType@.
--
-- The length of @paths@ should be @COMPSIZE(numPaths,pathNameType,paths)@.
--
-- The length of @transformValues@ should be @COMPSIZE(numPaths,transformType)@.


glCoverFillPathInstancedNV :: MonadIO m => GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glCoverFillPathInstancedNV = ffisizeienumPtrVuintenumenumPtrfloatIOV glCoverFillPathInstancedNVFunPtr

glCoverFillPathInstancedNVFunPtr :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glCoverFillPathInstancedNVFunPtr = unsafePerformIO (getProcAddress "glCoverFillPathInstancedNV")

{-# NOINLINE glCoverFillPathInstancedNVFunPtr #-}

-- | Usage: @'glCoverFillPathNV' path coverMode@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @coverMode@ is a @PathCoverMode@.


glCoverFillPathNV :: MonadIO m => GLuint -> GLenum -> m ()
glCoverFillPathNV = ffiuintenumIOV glCoverFillPathNVFunPtr

glCoverFillPathNVFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glCoverFillPathNVFunPtr = unsafePerformIO (getProcAddress "glCoverFillPathNV")

{-# NOINLINE glCoverFillPathNVFunPtr #-}

-- | Usage: @'glCoverStrokePathInstancedNV' numPaths pathNameType paths pathBase coverMode transformType transformValues@
--
-- The parameter @pathNameType@ is a @PathElementType@.
--
-- The parameter @paths@ is a @PathElement@.
--
-- The parameter @pathBase@ is a @Path@.
--
-- The parameter @coverMode@ is a @PathCoverMode@.
--
-- The parameter @transformType@ is a @PathTransformType@.
--
-- The length of @paths@ should be @COMPSIZE(numPaths,pathNameType,paths)@.
--
-- The length of @transformValues@ should be @COMPSIZE(numPaths,transformType)@.


glCoverStrokePathInstancedNV :: MonadIO m => GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glCoverStrokePathInstancedNV = ffisizeienumPtrVuintenumenumPtrfloatIOV glCoverStrokePathInstancedNVFunPtr

glCoverStrokePathInstancedNVFunPtr :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glCoverStrokePathInstancedNVFunPtr = unsafePerformIO (getProcAddress "glCoverStrokePathInstancedNV")

{-# NOINLINE glCoverStrokePathInstancedNVFunPtr #-}

-- | Usage: @'glCoverStrokePathNV' path coverMode@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @coverMode@ is a @PathCoverMode@.


glCoverStrokePathNV :: MonadIO m => GLuint -> GLenum -> m ()
glCoverStrokePathNV = ffiuintenumIOV glCoverStrokePathNVFunPtr

glCoverStrokePathNVFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glCoverStrokePathNVFunPtr = unsafePerformIO (getProcAddress "glCoverStrokePathNV")

{-# NOINLINE glCoverStrokePathNVFunPtr #-}

-- | Usage: @'glDeletePathsNV' path range@
--
-- The parameter @path@ is a @Path@.


glDeletePathsNV :: MonadIO m => GLuint -> GLsizei -> m ()
glDeletePathsNV = ffiuintsizeiIOV glDeletePathsNVFunPtr

glDeletePathsNVFunPtr :: FunPtr (GLuint -> GLsizei -> IO ())
glDeletePathsNVFunPtr = unsafePerformIO (getProcAddress "glDeletePathsNV")

{-# NOINLINE glDeletePathsNVFunPtr #-}

-- | Usage: @'glGenPathsNV' range@


glGenPathsNV :: MonadIO m => GLsizei -> m GLuint
glGenPathsNV = ffisizeiIOuint glGenPathsNVFunPtr

glGenPathsNVFunPtr :: FunPtr (GLsizei -> IO GLuint)
glGenPathsNVFunPtr = unsafePerformIO (getProcAddress "glGenPathsNV")

{-# NOINLINE glGenPathsNVFunPtr #-}

-- | Usage: @'glGetPathColorGenfvNV' color pname value@
--
-- The parameter @color@ is a @PathColor@.
--
-- The parameter @pname@ is a @PathGenMode@.
--
-- The length of @value@ should be @COMPSIZE(pname)@.


glGetPathColorGenfvNV :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetPathColorGenfvNV = ffienumenumPtrfloatIOV glGetPathColorGenfvNVFunPtr

glGetPathColorGenfvNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetPathColorGenfvNVFunPtr = unsafePerformIO (getProcAddress "glGetPathColorGenfvNV")

{-# NOINLINE glGetPathColorGenfvNVFunPtr #-}

-- | Usage: @'glGetPathColorGenivNV' color pname value@
--
-- The parameter @color@ is a @PathColor@.
--
-- The parameter @pname@ is a @PathGenMode@.
--
-- The length of @value@ should be @COMPSIZE(pname)@.


glGetPathColorGenivNV :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetPathColorGenivNV = ffienumenumPtrintIOV glGetPathColorGenivNVFunPtr

glGetPathColorGenivNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetPathColorGenivNVFunPtr = unsafePerformIO (getProcAddress "glGetPathColorGenivNV")

{-# NOINLINE glGetPathColorGenivNVFunPtr #-}

-- | Usage: @'glGetPathCommandsNV' path commands@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @commands@ is a @PathCommand@.
--
-- The length of @commands@ should be @COMPSIZE(path)@.


glGetPathCommandsNV :: MonadIO m => GLuint -> Ptr GLubyte -> m ()
glGetPathCommandsNV = ffiuintPtrubyteIOV glGetPathCommandsNVFunPtr

glGetPathCommandsNVFunPtr :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
glGetPathCommandsNVFunPtr = unsafePerformIO (getProcAddress "glGetPathCommandsNV")

{-# NOINLINE glGetPathCommandsNVFunPtr #-}

-- | Usage: @'glGetPathCoordsNV' path coords@
--
-- The parameter @path@ is a @Path@.
--
-- The length of @coords@ should be @COMPSIZE(path)@.


glGetPathCoordsNV :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glGetPathCoordsNV = ffiuintPtrfloatIOV glGetPathCoordsNVFunPtr

glGetPathCoordsNVFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glGetPathCoordsNVFunPtr = unsafePerformIO (getProcAddress "glGetPathCoordsNV")

{-# NOINLINE glGetPathCoordsNVFunPtr #-}

-- | Usage: @'glGetPathDashArrayNV' path dashArray@
--
-- The parameter @path@ is a @Path@.
--
-- The length of @dashArray@ should be @COMPSIZE(path)@.


glGetPathDashArrayNV :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glGetPathDashArrayNV = ffiuintPtrfloatIOV glGetPathDashArrayNVFunPtr

glGetPathDashArrayNVFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glGetPathDashArrayNVFunPtr = unsafePerformIO (getProcAddress "glGetPathDashArrayNV")

{-# NOINLINE glGetPathDashArrayNVFunPtr #-}

-- | Usage: @'glGetPathLengthNV' path startSegment numSegments@
--
-- The parameter @path@ is a @Path@.


glGetPathLengthNV :: MonadIO m => GLuint -> GLsizei -> GLsizei -> m GLfloat
glGetPathLengthNV = ffiuintsizeisizeiIOfloat glGetPathLengthNVFunPtr

glGetPathLengthNVFunPtr :: FunPtr (GLuint -> GLsizei -> GLsizei -> IO GLfloat)
glGetPathLengthNVFunPtr = unsafePerformIO (getProcAddress "glGetPathLengthNV")

{-# NOINLINE glGetPathLengthNVFunPtr #-}

-- | Usage: @'glGetPathMetricRangeNV' metricQueryMask firstPathName numPaths stride metrics@
--
-- The parameter @metricQueryMask@ is a @PathMetricMask@.
--
-- The parameter @firstPathName@ is a @Path@.
--
-- The length of @metrics@ should be @COMPSIZE(metricQueryMask,numPaths,stride)@.


glGetPathMetricRangeNV :: MonadIO m => GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> m ()
glGetPathMetricRangeNV = ffibitfielduintsizeisizeiPtrfloatIOV glGetPathMetricRangeNVFunPtr

glGetPathMetricRangeNVFunPtr :: FunPtr (GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> IO ())
glGetPathMetricRangeNVFunPtr = unsafePerformIO (getProcAddress "glGetPathMetricRangeNV")

{-# NOINLINE glGetPathMetricRangeNVFunPtr #-}

-- | Usage: @'glGetPathMetricsNV' metricQueryMask numPaths pathNameType paths pathBase stride metrics@
--
-- The parameter @metricQueryMask@ is a @PathMetricMask@.
--
-- The parameter @pathNameType@ is a @PathElementType@.
--
-- The parameter @paths@ is a @PathElement@.
--
-- The parameter @pathBase@ is a @Path@.
--
-- The length of @paths@ should be @COMPSIZE(numPaths,pathNameType,paths)@.
--
-- The length of @metrics@ should be @COMPSIZE(metricQueryMask,numPaths,stride)@.


glGetPathMetricsNV :: MonadIO m => GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glGetPathMetricsNV = ffibitfieldsizeienumPtrVuintsizeiPtrfloatIOV glGetPathMetricsNVFunPtr

glGetPathMetricsNVFunPtr :: FunPtr (GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glGetPathMetricsNVFunPtr = unsafePerformIO (getProcAddress "glGetPathMetricsNV")

{-# NOINLINE glGetPathMetricsNVFunPtr #-}

-- | Usage: @'glGetPathParameterfvNV' path pname value@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @pname@ is a @PathParameter@.
--
-- The length of @value@ should be @4@.


glGetPathParameterfvNV :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetPathParameterfvNV = ffiuintenumPtrfloatIOV glGetPathParameterfvNVFunPtr

glGetPathParameterfvNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetPathParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetPathParameterfvNV")

{-# NOINLINE glGetPathParameterfvNVFunPtr #-}

-- | Usage: @'glGetPathParameterivNV' path pname value@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @pname@ is a @PathParameter@.
--
-- The length of @value@ should be @4@.


glGetPathParameterivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetPathParameterivNV = ffiuintenumPtrintIOV glGetPathParameterivNVFunPtr

glGetPathParameterivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetPathParameterivNVFunPtr = unsafePerformIO (getProcAddress "glGetPathParameterivNV")

{-# NOINLINE glGetPathParameterivNVFunPtr #-}

-- | Usage: @'glGetPathSpacingNV' pathListMode numPaths pathNameType paths pathBase advanceScale kerningScale transformType returnedSpacing@
--
-- The parameter @pathListMode@ is a @PathListMode@.
--
-- The parameter @pathNameType@ is a @PathElementType@.
--
-- The parameter @paths@ is a @PathElement@.
--
-- The parameter @pathBase@ is a @Path@.
--
-- The parameter @transformType@ is a @PathTransformType@.
--
-- The length of @paths@ should be @COMPSIZE(numPaths,pathNameType,paths)@.
--
-- The length of @returnedSpacing@ should be @COMPSIZE(pathListMode,numPaths)@.


glGetPathSpacingNV :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> m ()
glGetPathSpacingNV = ffienumsizeienumPtrVuintfloatfloatenumPtrfloatIOV glGetPathSpacingNVFunPtr

glGetPathSpacingNVFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> IO ())
glGetPathSpacingNVFunPtr = unsafePerformIO (getProcAddress "glGetPathSpacingNV")

{-# NOINLINE glGetPathSpacingNVFunPtr #-}

-- | Usage: @'glGetPathTexGenfvNV' texCoordSet pname value@
--
-- The parameter @texCoordSet@ is a @TextureUnit@.
--
-- The parameter @pname@ is a @PathGenMode@.
--
-- The length of @value@ should be @COMPSIZE(pname)@.


glGetPathTexGenfvNV :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetPathTexGenfvNV = ffienumenumPtrfloatIOV glGetPathTexGenfvNVFunPtr

glGetPathTexGenfvNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetPathTexGenfvNVFunPtr = unsafePerformIO (getProcAddress "glGetPathTexGenfvNV")

{-# NOINLINE glGetPathTexGenfvNVFunPtr #-}

-- | Usage: @'glGetPathTexGenivNV' texCoordSet pname value@
--
-- The parameter @texCoordSet@ is a @TextureUnit@.
--
-- The parameter @pname@ is a @PathGenMode@.
--
-- The length of @value@ should be @COMPSIZE(pname)@.


glGetPathTexGenivNV :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetPathTexGenivNV = ffienumenumPtrintIOV glGetPathTexGenivNVFunPtr

glGetPathTexGenivNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetPathTexGenivNVFunPtr = unsafePerformIO (getProcAddress "glGetPathTexGenivNV")

{-# NOINLINE glGetPathTexGenivNVFunPtr #-}

-- | Usage: @'glGetProgramResourcefvNV' program programInterface index propCount props bufSize length params@


glGetProgramResourcefvNV :: MonadIO m => GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> m ()
glGetProgramResourcefvNV = ffiuintenumuintsizeiPtrenumsizeiPtrsizeiPtrfloatIOV glGetProgramResourcefvNVFunPtr

glGetProgramResourcefvNVFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> IO ())
glGetProgramResourcefvNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramResourcefvNV")

{-# NOINLINE glGetProgramResourcefvNVFunPtr #-}

-- | Usage: @'glInterpolatePathsNV' resultPath pathA pathB weight@
--
-- The parameter @resultPath@ is a @Path@.
--
-- The parameter @pathA@ is a @Path@.
--
-- The parameter @pathB@ is a @Path@.


glInterpolatePathsNV :: MonadIO m => GLuint -> GLuint -> GLuint -> GLfloat -> m ()
glInterpolatePathsNV = ffiuintuintuintfloatIOV glInterpolatePathsNVFunPtr

glInterpolatePathsNVFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLfloat -> IO ())
glInterpolatePathsNVFunPtr = unsafePerformIO (getProcAddress "glInterpolatePathsNV")

{-# NOINLINE glInterpolatePathsNVFunPtr #-}

-- | Usage: @'glIsPathNV' path@
--
-- The parameter @path@ is a @Path@.


glIsPathNV :: MonadIO m => GLuint -> m GLboolean
glIsPathNV = ffiuintIOboolean glIsPathNVFunPtr

glIsPathNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsPathNVFunPtr = unsafePerformIO (getProcAddress "glIsPathNV")

{-# NOINLINE glIsPathNVFunPtr #-}

-- | Usage: @'glIsPointInFillPathNV' path mask x y@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @mask@ is a @MaskedStencilValue@.


glIsPointInFillPathNV :: MonadIO m => GLuint -> GLuint -> GLfloat -> GLfloat -> m GLboolean
glIsPointInFillPathNV = ffiuintuintfloatfloatIOboolean glIsPointInFillPathNVFunPtr

glIsPointInFillPathNVFunPtr :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> IO GLboolean)
glIsPointInFillPathNVFunPtr = unsafePerformIO (getProcAddress "glIsPointInFillPathNV")

{-# NOINLINE glIsPointInFillPathNVFunPtr #-}

-- | Usage: @'glIsPointInStrokePathNV' path x y@
--
-- The parameter @path@ is a @Path@.


glIsPointInStrokePathNV :: MonadIO m => GLuint -> GLfloat -> GLfloat -> m GLboolean
glIsPointInStrokePathNV = ffiuintfloatfloatIOboolean glIsPointInStrokePathNVFunPtr

glIsPointInStrokePathNVFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO GLboolean)
glIsPointInStrokePathNVFunPtr = unsafePerformIO (getProcAddress "glIsPointInStrokePathNV")

{-# NOINLINE glIsPointInStrokePathNVFunPtr #-}

-- | Usage: @'glMatrixLoad3x2fNV' matrixMode m@


glMatrixLoad3x2fNV :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixLoad3x2fNV = ffienumPtrfloatIOV glMatrixLoad3x2fNVFunPtr

glMatrixLoad3x2fNVFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixLoad3x2fNVFunPtr = unsafePerformIO (getProcAddress "glMatrixLoad3x2fNV")

{-# NOINLINE glMatrixLoad3x2fNVFunPtr #-}

-- | Usage: @'glMatrixLoad3x3fNV' matrixMode m@


glMatrixLoad3x3fNV :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixLoad3x3fNV = ffienumPtrfloatIOV glMatrixLoad3x3fNVFunPtr

glMatrixLoad3x3fNVFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixLoad3x3fNVFunPtr = unsafePerformIO (getProcAddress "glMatrixLoad3x3fNV")

{-# NOINLINE glMatrixLoad3x3fNVFunPtr #-}

-- | Usage: @'glMatrixLoadTranspose3x3fNV' matrixMode m@


glMatrixLoadTranspose3x3fNV :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixLoadTranspose3x3fNV = ffienumPtrfloatIOV glMatrixLoadTranspose3x3fNVFunPtr

glMatrixLoadTranspose3x3fNVFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixLoadTranspose3x3fNVFunPtr = unsafePerformIO (getProcAddress "glMatrixLoadTranspose3x3fNV")

{-# NOINLINE glMatrixLoadTranspose3x3fNVFunPtr #-}

-- | Usage: @'glMatrixMult3x2fNV' matrixMode m@


glMatrixMult3x2fNV :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixMult3x2fNV = ffienumPtrfloatIOV glMatrixMult3x2fNVFunPtr

glMatrixMult3x2fNVFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixMult3x2fNVFunPtr = unsafePerformIO (getProcAddress "glMatrixMult3x2fNV")

{-# NOINLINE glMatrixMult3x2fNVFunPtr #-}

-- | Usage: @'glMatrixMult3x3fNV' matrixMode m@


glMatrixMult3x3fNV :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixMult3x3fNV = ffienumPtrfloatIOV glMatrixMult3x3fNVFunPtr

glMatrixMult3x3fNVFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixMult3x3fNVFunPtr = unsafePerformIO (getProcAddress "glMatrixMult3x3fNV")

{-# NOINLINE glMatrixMult3x3fNVFunPtr #-}

-- | Usage: @'glMatrixMultTranspose3x3fNV' matrixMode m@


glMatrixMultTranspose3x3fNV :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixMultTranspose3x3fNV = ffienumPtrfloatIOV glMatrixMultTranspose3x3fNVFunPtr

glMatrixMultTranspose3x3fNVFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixMultTranspose3x3fNVFunPtr = unsafePerformIO (getProcAddress "glMatrixMultTranspose3x3fNV")

{-# NOINLINE glMatrixMultTranspose3x3fNVFunPtr #-}

-- | Usage: @'glPathColorGenNV' color genMode colorFormat coeffs@
--
-- The parameter @color@ is a @PathColor@.
--
-- The parameter @genMode@ is a @PathGenMode@.
--
-- The parameter @colorFormat@ is a @PathColorFormat@.
--
-- The length of @coeffs@ should be @COMPSIZE(genMode,colorFormat)@.


glPathColorGenNV :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glPathColorGenNV = ffienumenumenumPtrfloatIOV glPathColorGenNVFunPtr

glPathColorGenNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glPathColorGenNVFunPtr = unsafePerformIO (getProcAddress "glPathColorGenNV")

{-# NOINLINE glPathColorGenNVFunPtr #-}

-- | Usage: @'glPathCommandsNV' path numCommands commands numCoords coordType coords@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @commands@ is a @PathCommand@.
--
-- The parameter @coordType@ is a @PathCoordType@.
--
-- The length of @commands@ should be @numCommands@.
--
-- The length of @coords@ should be @COMPSIZE(numCoords,coordType)@.


glPathCommandsNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> m ()
glPathCommandsNV = ffiuintsizeiPtrubytesizeienumPtrVIOV glPathCommandsNVFunPtr

glPathCommandsNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ())
glPathCommandsNVFunPtr = unsafePerformIO (getProcAddress "glPathCommandsNV")

{-# NOINLINE glPathCommandsNVFunPtr #-}

-- | Usage: @'glPathCoordsNV' path numCoords coordType coords@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @coordType@ is a @PathCoordType@.
--
-- The length of @coords@ should be @COMPSIZE(numCoords,coordType)@.


glPathCoordsNV :: MonadIO m => GLuint -> GLsizei -> GLenum -> Ptr () -> m ()
glPathCoordsNV = ffiuintsizeienumPtrVIOV glPathCoordsNVFunPtr

glPathCoordsNVFunPtr :: FunPtr (GLuint -> GLsizei -> GLenum -> Ptr () -> IO ())
glPathCoordsNVFunPtr = unsafePerformIO (getProcAddress "glPathCoordsNV")

{-# NOINLINE glPathCoordsNVFunPtr #-}

-- | Usage: @'glPathCoverDepthFuncNV' func@
--
-- The parameter @func@ is a @DepthFunction@, one of: 'Graphics.GL.Internal.Shared.GL_ALWAYS', 'Graphics.GL.Internal.Shared.GL_EQUAL', 'Graphics.GL.Internal.Shared.GL_GEQUAL', 'Graphics.GL.Internal.Shared.GL_GREATER', 'Graphics.GL.Internal.Shared.GL_LEQUAL', 'Graphics.GL.Internal.Shared.GL_LESS', 'Graphics.GL.Internal.Shared.GL_NEVER', 'Graphics.GL.Internal.Shared.GL_NOTEQUAL'.


glPathCoverDepthFuncNV :: MonadIO m => GLenum -> m ()
glPathCoverDepthFuncNV = ffienumIOV glPathCoverDepthFuncNVFunPtr

glPathCoverDepthFuncNVFunPtr :: FunPtr (GLenum -> IO ())
glPathCoverDepthFuncNVFunPtr = unsafePerformIO (getProcAddress "glPathCoverDepthFuncNV")

{-# NOINLINE glPathCoverDepthFuncNVFunPtr #-}

-- | Usage: @'glPathDashArrayNV' path dashCount dashArray@
--
-- The parameter @path@ is a @Path@.
--
-- The length of @dashArray@ should be @dashCount@.


glPathDashArrayNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glPathDashArrayNV = ffiuintsizeiPtrfloatIOV glPathDashArrayNVFunPtr

glPathDashArrayNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glPathDashArrayNVFunPtr = unsafePerformIO (getProcAddress "glPathDashArrayNV")

{-# NOINLINE glPathDashArrayNVFunPtr #-}

-- | Usage: @'glPathFogGenNV' genMode@
--
-- The parameter @genMode@ is a @PathGenMode@.


glPathFogGenNV :: MonadIO m => GLenum -> m ()
glPathFogGenNV = ffienumIOV glPathFogGenNVFunPtr

glPathFogGenNVFunPtr :: FunPtr (GLenum -> IO ())
glPathFogGenNVFunPtr = unsafePerformIO (getProcAddress "glPathFogGenNV")

{-# NOINLINE glPathFogGenNVFunPtr #-}

-- | Usage: @'glPathGlyphIndexArrayNV' firstPathName fontTarget fontName fontStyle firstGlyphIndex numGlyphs pathParameterTemplate emScale@


glPathGlyphIndexArrayNV :: MonadIO m => GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> m GLenum
glPathGlyphIndexArrayNV = ffiuintenumPtrVbitfielduintsizeiuintfloatIOenum glPathGlyphIndexArrayNVFunPtr

glPathGlyphIndexArrayNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum)
glPathGlyphIndexArrayNVFunPtr = unsafePerformIO (getProcAddress "glPathGlyphIndexArrayNV")

{-# NOINLINE glPathGlyphIndexArrayNVFunPtr #-}

-- | Usage: @'glPathGlyphIndexRangeNV' fontTarget fontName fontStyle pathParameterTemplate emScale baseAndCount@


glPathGlyphIndexRangeNV :: MonadIO m => GLenum -> Ptr () -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> m GLenum
glPathGlyphIndexRangeNV = ffienumPtrVbitfielduintfloatPtruintIOenum glPathGlyphIndexRangeNVFunPtr

glPathGlyphIndexRangeNVFunPtr :: FunPtr (GLenum -> Ptr () -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> IO GLenum)
glPathGlyphIndexRangeNVFunPtr = unsafePerformIO (getProcAddress "glPathGlyphIndexRangeNV")

{-# NOINLINE glPathGlyphIndexRangeNVFunPtr #-}

-- | Usage: @'glPathGlyphRangeNV' firstPathName fontTarget fontName fontStyle firstGlyph numGlyphs handleMissingGlyphs pathParameterTemplate emScale@
--
-- The parameter @firstPathName@ is a @Path@.
--
-- The parameter @fontTarget@ is a @PathFontTarget@.
--
-- The parameter @fontStyle@ is a @PathFontStyle@.
--
-- The parameter @handleMissingGlyphs@ is a @PathHandleMissingGlyphs@.
--
-- The parameter @pathParameterTemplate@ is a @Path@.
--
-- The length of @fontName@ should be @COMPSIZE(fontTarget,fontName)@.


glPathGlyphRangeNV :: MonadIO m => GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> m ()
glPathGlyphRangeNV = ffiuintenumPtrVbitfielduintsizeienumuintfloatIOV glPathGlyphRangeNVFunPtr

glPathGlyphRangeNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> IO ())
glPathGlyphRangeNVFunPtr = unsafePerformIO (getProcAddress "glPathGlyphRangeNV")

{-# NOINLINE glPathGlyphRangeNVFunPtr #-}

-- | Usage: @'glPathGlyphsNV' firstPathName fontTarget fontName fontStyle numGlyphs type charcodes handleMissingGlyphs pathParameterTemplate emScale@
--
-- The parameter @firstPathName@ is a @Path@.
--
-- The parameter @fontTarget@ is a @PathFontTarget@.
--
-- The parameter @fontStyle@ is a @PathFontStyle@.
--
-- The parameter @type@ is a @PathElementType@.
--
-- The parameter @handleMissingGlyphs@ is a @PathHandleMissingGlyphs@.
--
-- The parameter @pathParameterTemplate@ is a @Path@.
--
-- The length of @fontName@ should be @COMPSIZE(fontTarget,fontName)@.
--
-- The length of @charcodes@ should be @COMPSIZE(numGlyphs,type,charcodes)@.


glPathGlyphsNV :: MonadIO m => GLuint -> GLenum -> Ptr () -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLenum -> GLuint -> GLfloat -> m ()
glPathGlyphsNV = ffiuintenumPtrVbitfieldsizeienumPtrVenumuintfloatIOV glPathGlyphsNVFunPtr

glPathGlyphsNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLenum -> GLuint -> GLfloat -> IO ())
glPathGlyphsNVFunPtr = unsafePerformIO (getProcAddress "glPathGlyphsNV")

{-# NOINLINE glPathGlyphsNVFunPtr #-}

-- | Usage: @'glPathMemoryGlyphIndexArrayNV' firstPathName fontTarget fontSize fontData faceIndex firstGlyphIndex numGlyphs pathParameterTemplate emScale@


glPathMemoryGlyphIndexArrayNV :: MonadIO m => GLuint -> GLenum -> GLsizeiptr -> Ptr () -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> m GLenum
glPathMemoryGlyphIndexArrayNV = ffiuintenumsizeiptrPtrVsizeiuintsizeiuintfloatIOenum glPathMemoryGlyphIndexArrayNVFunPtr

glPathMemoryGlyphIndexArrayNVFunPtr :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> Ptr () -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum)
glPathMemoryGlyphIndexArrayNVFunPtr = unsafePerformIO (getProcAddress "glPathMemoryGlyphIndexArrayNV")

{-# NOINLINE glPathMemoryGlyphIndexArrayNVFunPtr #-}

-- | Usage: @'glPathParameterfNV' path pname value@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @pname@ is a @PathParameter@.


glPathParameterfNV :: MonadIO m => GLuint -> GLenum -> GLfloat -> m ()
glPathParameterfNV = ffiuintenumfloatIOV glPathParameterfNVFunPtr

glPathParameterfNVFunPtr :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
glPathParameterfNVFunPtr = unsafePerformIO (getProcAddress "glPathParameterfNV")

{-# NOINLINE glPathParameterfNVFunPtr #-}

-- | Usage: @'glPathParameterfvNV' path pname value@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @pname@ is a @PathParameter@.
--
-- The length of @value@ should be @COMPSIZE(pname)@.


glPathParameterfvNV :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glPathParameterfvNV = ffiuintenumPtrfloatIOV glPathParameterfvNVFunPtr

glPathParameterfvNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glPathParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glPathParameterfvNV")

{-# NOINLINE glPathParameterfvNVFunPtr #-}

-- | Usage: @'glPathParameteriNV' path pname value@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @pname@ is a @PathParameter@.


glPathParameteriNV :: MonadIO m => GLuint -> GLenum -> GLint -> m ()
glPathParameteriNV = ffiuintenumintIOV glPathParameteriNVFunPtr

glPathParameteriNVFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
glPathParameteriNVFunPtr = unsafePerformIO (getProcAddress "glPathParameteriNV")

{-# NOINLINE glPathParameteriNVFunPtr #-}

-- | Usage: @'glPathParameterivNV' path pname value@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @pname@ is a @PathParameter@.
--
-- The length of @value@ should be @COMPSIZE(pname)@.


glPathParameterivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glPathParameterivNV = ffiuintenumPtrintIOV glPathParameterivNVFunPtr

glPathParameterivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glPathParameterivNVFunPtr = unsafePerformIO (getProcAddress "glPathParameterivNV")

{-# NOINLINE glPathParameterivNVFunPtr #-}

-- | Usage: @'glPathStencilDepthOffsetNV' factor units@


glPathStencilDepthOffsetNV :: MonadIO m => GLfloat -> GLfloat -> m ()
glPathStencilDepthOffsetNV = ffifloatfloatIOV glPathStencilDepthOffsetNVFunPtr

glPathStencilDepthOffsetNVFunPtr :: FunPtr (GLfloat -> GLfloat -> IO ())
glPathStencilDepthOffsetNVFunPtr = unsafePerformIO (getProcAddress "glPathStencilDepthOffsetNV")

{-# NOINLINE glPathStencilDepthOffsetNVFunPtr #-}

-- | Usage: @'glPathStencilFuncNV' func ref mask@
--
-- The parameter @func@ is a @StencilFunction@, one of: 'Graphics.GL.Internal.Shared.GL_ALWAYS', 'Graphics.GL.Internal.Shared.GL_EQUAL', 'Graphics.GL.Internal.Shared.GL_GEQUAL', 'Graphics.GL.Internal.Shared.GL_GREATER', 'Graphics.GL.Internal.Shared.GL_LEQUAL', 'Graphics.GL.Internal.Shared.GL_LESS', 'Graphics.GL.Internal.Shared.GL_NEVER', 'Graphics.GL.Internal.Shared.GL_NOTEQUAL'.
--
-- The parameter @ref@ is a @ClampedStencilValue@.
--
-- The parameter @mask@ is a @MaskedStencilValue@.


glPathStencilFuncNV :: MonadIO m => GLenum -> GLint -> GLuint -> m ()
glPathStencilFuncNV = ffienumintuintIOV glPathStencilFuncNVFunPtr

glPathStencilFuncNVFunPtr :: FunPtr (GLenum -> GLint -> GLuint -> IO ())
glPathStencilFuncNVFunPtr = unsafePerformIO (getProcAddress "glPathStencilFuncNV")

{-# NOINLINE glPathStencilFuncNVFunPtr #-}

-- | Usage: @'glPathStringNV' path format length pathString@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @format@ is a @PathStringFormat@.
--
-- The length of @pathString@ should be @length@.


glPathStringNV :: MonadIO m => GLuint -> GLenum -> GLsizei -> Ptr () -> m ()
glPathStringNV = ffiuintenumsizeiPtrVIOV glPathStringNVFunPtr

glPathStringNVFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr () -> IO ())
glPathStringNVFunPtr = unsafePerformIO (getProcAddress "glPathStringNV")

{-# NOINLINE glPathStringNVFunPtr #-}

-- | Usage: @'glPathSubCommandsNV' path commandStart commandsToDelete numCommands commands numCoords coordType coords@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @commands@ is a @PathCommand@.
--
-- The parameter @coordType@ is a @PathCoordType@.
--
-- The length of @commands@ should be @numCommands@.
--
-- The length of @coords@ should be @COMPSIZE(numCoords,coordType)@.


glPathSubCommandsNV :: MonadIO m => GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> m ()
glPathSubCommandsNV = ffiuintsizeisizeisizeiPtrubytesizeienumPtrVIOV glPathSubCommandsNVFunPtr

glPathSubCommandsNVFunPtr :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ())
glPathSubCommandsNVFunPtr = unsafePerformIO (getProcAddress "glPathSubCommandsNV")

{-# NOINLINE glPathSubCommandsNVFunPtr #-}

-- | Usage: @'glPathSubCoordsNV' path coordStart numCoords coordType coords@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @coordType@ is a @PathCoordType@.
--
-- The length of @coords@ should be @COMPSIZE(numCoords,coordType)@.


glPathSubCoordsNV :: MonadIO m => GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr () -> m ()
glPathSubCoordsNV = ffiuintsizeisizeienumPtrVIOV glPathSubCoordsNVFunPtr

glPathSubCoordsNVFunPtr :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr () -> IO ())
glPathSubCoordsNVFunPtr = unsafePerformIO (getProcAddress "glPathSubCoordsNV")

{-# NOINLINE glPathSubCoordsNVFunPtr #-}

-- | Usage: @'glPathTexGenNV' texCoordSet genMode components coeffs@
--
-- The parameter @texCoordSet@ is a @PathColor@.
--
-- The parameter @genMode@ is a @PathGenMode@.
--
-- The length of @coeffs@ should be @COMPSIZE(genMode,components)@.


glPathTexGenNV :: MonadIO m => GLenum -> GLenum -> GLint -> Ptr GLfloat -> m ()
glPathTexGenNV = ffienumenumintPtrfloatIOV glPathTexGenNVFunPtr

glPathTexGenNVFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> Ptr GLfloat -> IO ())
glPathTexGenNVFunPtr = unsafePerformIO (getProcAddress "glPathTexGenNV")

{-# NOINLINE glPathTexGenNVFunPtr #-}

-- | Usage: @'glPointAlongPathNV' path startSegment numSegments distance x y tangentX tangentY@
--
-- The parameter @path@ is a @Path@.
--
-- The length of @x@ should be @1@.
--
-- The length of @y@ should be @1@.
--
-- The length of @tangentX@ should be @1@.
--
-- The length of @tangentY@ should be @1@.


glPointAlongPathNV :: MonadIO m => GLuint -> GLsizei -> GLsizei -> GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m GLboolean
glPointAlongPathNV = ffiuintsizeisizeifloatPtrfloatPtrfloatPtrfloatPtrfloatIOboolean glPointAlongPathNVFunPtr

glPointAlongPathNVFunPtr :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO GLboolean)
glPointAlongPathNVFunPtr = unsafePerformIO (getProcAddress "glPointAlongPathNV")

{-# NOINLINE glPointAlongPathNVFunPtr #-}

-- | Usage: @'glProgramPathFragmentInputGenNV' program location genMode components coeffs@


glProgramPathFragmentInputGenNV :: MonadIO m => GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> m ()
glProgramPathFragmentInputGenNV = ffiuintintenumintPtrfloatIOV glProgramPathFragmentInputGenNVFunPtr

glProgramPathFragmentInputGenNVFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> IO ())
glProgramPathFragmentInputGenNVFunPtr = unsafePerformIO (getProcAddress "glProgramPathFragmentInputGenNV")

{-# NOINLINE glProgramPathFragmentInputGenNVFunPtr #-}

-- | Usage: @'glStencilFillPathInstancedNV' numPaths pathNameType paths pathBase fillMode mask transformType transformValues@
--
-- The parameter @pathNameType@ is a @PathElementType@.
--
-- The parameter @paths@ is a @PathElement@.
--
-- The parameter @pathBase@ is a @Path@.
--
-- The parameter @fillMode@ is a @PathFillMode@.
--
-- The parameter @mask@ is a @MaskedStencilValue@.
--
-- The parameter @transformType@ is a @PathTransformType@.
--
-- The length of @paths@ should be @COMPSIZE(numPaths,pathNameType,paths)@.
--
-- The length of @transformValues@ should be @COMPSIZE(numPaths,transformType)@.


glStencilFillPathInstancedNV :: MonadIO m => GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> m ()
glStencilFillPathInstancedNV = ffisizeienumPtrVuintenumuintenumPtrfloatIOV glStencilFillPathInstancedNVFunPtr

glStencilFillPathInstancedNVFunPtr :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
glStencilFillPathInstancedNVFunPtr = unsafePerformIO (getProcAddress "glStencilFillPathInstancedNV")

{-# NOINLINE glStencilFillPathInstancedNVFunPtr #-}

-- | Usage: @'glStencilFillPathNV' path fillMode mask@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @fillMode@ is a @PathFillMode@.
--
-- The parameter @mask@ is a @MaskedStencilValue@.


glStencilFillPathNV :: MonadIO m => GLuint -> GLenum -> GLuint -> m ()
glStencilFillPathNV = ffiuintenumuintIOV glStencilFillPathNVFunPtr

glStencilFillPathNVFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> IO ())
glStencilFillPathNVFunPtr = unsafePerformIO (getProcAddress "glStencilFillPathNV")

{-# NOINLINE glStencilFillPathNVFunPtr #-}

-- | Usage: @'glStencilStrokePathInstancedNV' numPaths pathNameType paths pathBase reference mask transformType transformValues@
--
-- The parameter @pathNameType@ is a @PathElementType@.
--
-- The parameter @paths@ is a @PathElement@.
--
-- The parameter @pathBase@ is a @Path@.
--
-- The parameter @reference@ is a @StencilValue@.
--
-- The parameter @mask@ is a @MaskedStencilValue@.
--
-- The parameter @transformType@ is a @PathTransformType@.
--
-- The length of @paths@ should be @COMPSIZE(numPaths,pathNameType,paths)@.
--
-- The length of @transformValues@ should be @COMPSIZE(numPaths,transformType)@.


glStencilStrokePathInstancedNV :: MonadIO m => GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> Ptr GLfloat -> m ()
glStencilStrokePathInstancedNV = ffisizeienumPtrVuintintuintenumPtrfloatIOV glStencilStrokePathInstancedNVFunPtr

glStencilStrokePathInstancedNVFunPtr :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
glStencilStrokePathInstancedNVFunPtr = unsafePerformIO (getProcAddress "glStencilStrokePathInstancedNV")

{-# NOINLINE glStencilStrokePathInstancedNVFunPtr #-}

-- | Usage: @'glStencilStrokePathNV' path reference mask@
--
-- The parameter @path@ is a @Path@.
--
-- The parameter @reference@ is a @StencilValue@.
--
-- The parameter @mask@ is a @MaskedStencilValue@.


glStencilStrokePathNV :: MonadIO m => GLuint -> GLint -> GLuint -> m ()
glStencilStrokePathNV = ffiuintintuintIOV glStencilStrokePathNVFunPtr

glStencilStrokePathNVFunPtr :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
glStencilStrokePathNVFunPtr = unsafePerformIO (getProcAddress "glStencilStrokePathNV")

{-# NOINLINE glStencilStrokePathNVFunPtr #-}

-- | Usage: @'glStencilThenCoverFillPathInstancedNV' numPaths pathNameType paths pathBase fillMode mask coverMode transformType transformValues@


glStencilThenCoverFillPathInstancedNV :: MonadIO m => GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glStencilThenCoverFillPathInstancedNV = ffisizeienumPtrVuintenumuintenumenumPtrfloatIOV glStencilThenCoverFillPathInstancedNVFunPtr

glStencilThenCoverFillPathInstancedNVFunPtr :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glStencilThenCoverFillPathInstancedNVFunPtr = unsafePerformIO (getProcAddress "glStencilThenCoverFillPathInstancedNV")

{-# NOINLINE glStencilThenCoverFillPathInstancedNVFunPtr #-}

-- | Usage: @'glStencilThenCoverFillPathNV' path fillMode mask coverMode@


glStencilThenCoverFillPathNV :: MonadIO m => GLuint -> GLenum -> GLuint -> GLenum -> m ()
glStencilThenCoverFillPathNV = ffiuintenumuintenumIOV glStencilThenCoverFillPathNVFunPtr

glStencilThenCoverFillPathNVFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> IO ())
glStencilThenCoverFillPathNVFunPtr = unsafePerformIO (getProcAddress "glStencilThenCoverFillPathNV")

{-# NOINLINE glStencilThenCoverFillPathNVFunPtr #-}

-- | Usage: @'glStencilThenCoverStrokePathInstancedNV' numPaths pathNameType paths pathBase reference mask coverMode transformType transformValues@


glStencilThenCoverStrokePathInstancedNV :: MonadIO m => GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glStencilThenCoverStrokePathInstancedNV = ffisizeienumPtrVuintintuintenumenumPtrfloatIOV glStencilThenCoverStrokePathInstancedNVFunPtr

glStencilThenCoverStrokePathInstancedNVFunPtr :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glStencilThenCoverStrokePathInstancedNVFunPtr = unsafePerformIO (getProcAddress "glStencilThenCoverStrokePathInstancedNV")

{-# NOINLINE glStencilThenCoverStrokePathInstancedNVFunPtr #-}

-- | Usage: @'glStencilThenCoverStrokePathNV' path reference mask coverMode@


glStencilThenCoverStrokePathNV :: MonadIO m => GLuint -> GLint -> GLuint -> GLenum -> m ()
glStencilThenCoverStrokePathNV = ffiuintintuintenumIOV glStencilThenCoverStrokePathNVFunPtr

glStencilThenCoverStrokePathNVFunPtr :: FunPtr (GLuint -> GLint -> GLuint -> GLenum -> IO ())
glStencilThenCoverStrokePathNVFunPtr = unsafePerformIO (getProcAddress "glStencilThenCoverStrokePathNV")

{-# NOINLINE glStencilThenCoverStrokePathNVFunPtr #-}

-- | Usage: @'glTransformPathNV' resultPath srcPath transformType transformValues@
--
-- The parameter @resultPath@ is a @Path@.
--
-- The parameter @srcPath@ is a @Path@.
--
-- The parameter @transformType@ is a @PathTransformType@.
--
-- The length of @transformValues@ should be @COMPSIZE(transformType)@.


glTransformPathNV :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLfloat -> m ()
glTransformPathNV = ffiuintuintenumPtrfloatIOV glTransformPathNVFunPtr

glTransformPathNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
glTransformPathNVFunPtr = unsafePerformIO (getProcAddress "glTransformPathNV")

{-# NOINLINE glTransformPathNVFunPtr #-}

-- | Usage: @'glWeightPathsNV' resultPath numPaths paths weights@
--
-- The parameter @resultPath@ is a @Path@.
--
-- The parameter @paths@ is a @Path@.
--
-- The length of @paths@ should be @numPaths@.
--
-- The length of @weights@ should be @numPaths@.


glWeightPathsNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> m ()
glWeightPathsNV = ffiuintsizeiPtruintPtrfloatIOV glWeightPathsNVFunPtr

glWeightPathsNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ())
glWeightPathsNVFunPtr = unsafePerformIO (getProcAddress "glWeightPathsNV")

{-# NOINLINE glWeightPathsNVFunPtr #-}

pattern GL_2_BYTES_NV  :: (Eq a, Num a) => a

pattern GL_2_BYTES_NV = 0x1407

pattern GL_3_BYTES_NV  :: (Eq a, Num a) => a

pattern GL_3_BYTES_NV = 0x1408

pattern GL_4_BYTES_NV  :: (Eq a, Num a) => a

pattern GL_4_BYTES_NV = 0x1409

pattern GL_ACCUM_ADJACENT_PAIRS_NV  :: (Eq a, Num a) => a

pattern GL_ACCUM_ADJACENT_PAIRS_NV = 0x90AD

pattern GL_ADJACENT_PAIRS_NV  :: (Eq a, Num a) => a

pattern GL_ADJACENT_PAIRS_NV = 0x90AE

pattern GL_AFFINE_2D_NV  :: (Eq a, Num a) => a

pattern GL_AFFINE_2D_NV = 0x9092

pattern GL_AFFINE_3D_NV  :: (Eq a, Num a) => a

pattern GL_AFFINE_3D_NV = 0x9094

pattern GL_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_ARC_TO_NV = 0xFE

pattern GL_BEVEL_NV  :: (Eq a, Num a) => a

pattern GL_BEVEL_NV = 0x90A6

pattern GL_BOLD_BIT_NV  :: (Eq a, Num a) => a

pattern GL_BOLD_BIT_NV = 0x01

pattern GL_BOUNDING_BOX_NV  :: (Eq a, Num a) => a

pattern GL_BOUNDING_BOX_NV = 0x908D

pattern GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV  :: (Eq a, Num a) => a

pattern GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV = 0x909C

pattern GL_CIRCULAR_CCW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_CIRCULAR_CCW_ARC_TO_NV = 0xF8

pattern GL_CIRCULAR_CW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_CIRCULAR_CW_ARC_TO_NV = 0xFA

pattern GL_CIRCULAR_TANGENT_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_CIRCULAR_TANGENT_ARC_TO_NV = 0xFC

pattern GL_CLOSE_PATH_NV  :: (Eq a, Num a) => a

pattern GL_CLOSE_PATH_NV = 0x00

pattern GL_CONIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_CONIC_CURVE_TO_NV = 0x1A

pattern GL_CONSTANT_NV  :: (Eq a, Num a) => a

pattern GL_CONSTANT_NV = 0x8576

pattern GL_CONVEX_HULL_NV  :: (Eq a, Num a) => a

pattern GL_CONVEX_HULL_NV = 0x908B

pattern GL_COUNT_DOWN_NV  :: (Eq a, Num a) => a

pattern GL_COUNT_DOWN_NV = 0x9089

pattern GL_COUNT_UP_NV  :: (Eq a, Num a) => a

pattern GL_COUNT_UP_NV = 0x9088

pattern GL_CUBIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_CUBIC_CURVE_TO_NV = 0x0C

pattern GL_DUP_FIRST_CUBIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_DUP_FIRST_CUBIC_CURVE_TO_NV = 0xF2

pattern GL_DUP_LAST_CUBIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_DUP_LAST_CUBIC_CURVE_TO_NV = 0xF4

pattern GL_EYE_LINEAR_NV  :: (Eq a, Num a) => a

pattern GL_EYE_LINEAR_NV = 0x2400

pattern GL_FILE_NAME_NV  :: (Eq a, Num a) => a

pattern GL_FILE_NAME_NV = 0x9074

pattern GL_FIRST_TO_REST_NV  :: (Eq a, Num a) => a

pattern GL_FIRST_TO_REST_NV = 0x90AF

pattern GL_FONT_ASCENDER_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_ASCENDER_BIT_NV = 0x00200000

pattern GL_FONT_DESCENDER_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_DESCENDER_BIT_NV = 0x00400000

pattern GL_FONT_GLYPHS_AVAILABLE_NV  :: (Eq a, Num a) => a

pattern GL_FONT_GLYPHS_AVAILABLE_NV = 0x9368

pattern GL_FONT_HAS_KERNING_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_HAS_KERNING_BIT_NV = 0x10000000

pattern GL_FONT_HEIGHT_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_HEIGHT_BIT_NV = 0x00800000

pattern GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV = 0x02000000

pattern GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV = 0x01000000

pattern GL_FONT_NUM_GLYPH_INDICES_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_NUM_GLYPH_INDICES_BIT_NV = 0x20000000

pattern GL_FONT_TARGET_UNAVAILABLE_NV  :: (Eq a, Num a) => a

pattern GL_FONT_TARGET_UNAVAILABLE_NV = 0x9369

pattern GL_FONT_UNAVAILABLE_NV  :: (Eq a, Num a) => a

pattern GL_FONT_UNAVAILABLE_NV = 0x936A

pattern GL_FONT_UNDERLINE_POSITION_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_UNDERLINE_POSITION_BIT_NV = 0x04000000

pattern GL_FONT_UNDERLINE_THICKNESS_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_UNDERLINE_THICKNESS_BIT_NV = 0x08000000

pattern GL_FONT_UNINTELLIGIBLE_NV  :: (Eq a, Num a) => a

pattern GL_FONT_UNINTELLIGIBLE_NV = 0x936B

pattern GL_FONT_UNITS_PER_EM_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_UNITS_PER_EM_BIT_NV = 0x00100000

pattern GL_FONT_X_MAX_BOUNDS_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_X_MAX_BOUNDS_BIT_NV = 0x00040000

pattern GL_FONT_X_MIN_BOUNDS_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_X_MIN_BOUNDS_BIT_NV = 0x00010000

pattern GL_FONT_Y_MAX_BOUNDS_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_Y_MAX_BOUNDS_BIT_NV = 0x00080000

pattern GL_FONT_Y_MIN_BOUNDS_BIT_NV  :: (Eq a, Num a) => a

pattern GL_FONT_Y_MIN_BOUNDS_BIT_NV = 0x00020000

pattern GL_FRAGMENT_INPUT_NV  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_INPUT_NV = 0x936D

pattern GL_GLYPH_HAS_KERNING_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_HAS_KERNING_BIT_NV = 0x100

pattern GL_GLYPH_HEIGHT_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_HEIGHT_BIT_NV = 0x02

pattern GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV = 0x10

pattern GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV = 0x04

pattern GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV = 0x08

pattern GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV = 0x80

pattern GL_GLYPH_VERTICAL_BEARING_X_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_VERTICAL_BEARING_X_BIT_NV = 0x20

pattern GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV = 0x40

pattern GL_GLYPH_WIDTH_BIT_NV  :: (Eq a, Num a) => a

pattern GL_GLYPH_WIDTH_BIT_NV = 0x01

pattern GL_HORIZONTAL_LINE_TO_NV  :: (Eq a, Num a) => a

pattern GL_HORIZONTAL_LINE_TO_NV = 0x06

pattern GL_ITALIC_BIT_NV  :: (Eq a, Num a) => a

pattern GL_ITALIC_BIT_NV = 0x02

pattern GL_LARGE_CCW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_LARGE_CCW_ARC_TO_NV = 0x16

pattern GL_LARGE_CW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_LARGE_CW_ARC_TO_NV = 0x18

pattern GL_LINE_TO_NV  :: (Eq a, Num a) => a

pattern GL_LINE_TO_NV = 0x04

pattern GL_MITER_REVERT_NV  :: (Eq a, Num a) => a

pattern GL_MITER_REVERT_NV = 0x90A7

pattern GL_MITER_TRUNCATE_NV  :: (Eq a, Num a) => a

pattern GL_MITER_TRUNCATE_NV = 0x90A8

pattern GL_MOVE_TO_CONTINUES_NV  :: (Eq a, Num a) => a

pattern GL_MOVE_TO_CONTINUES_NV = 0x90B6

pattern GL_MOVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_MOVE_TO_NV = 0x02

pattern GL_MOVE_TO_RESETS_NV  :: (Eq a, Num a) => a

pattern GL_MOVE_TO_RESETS_NV = 0x90B5

pattern GL_OBJECT_LINEAR_NV  :: (Eq a, Num a) => a

pattern GL_OBJECT_LINEAR_NV = 0x2401

pattern GL_PATH_CLIENT_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_PATH_CLIENT_LENGTH_NV = 0x907F

pattern GL_PATH_COMMAND_COUNT_NV  :: (Eq a, Num a) => a

pattern GL_PATH_COMMAND_COUNT_NV = 0x909D

pattern GL_PATH_COMPUTED_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_PATH_COMPUTED_LENGTH_NV = 0x90A0

pattern GL_PATH_COORD_COUNT_NV  :: (Eq a, Num a) => a

pattern GL_PATH_COORD_COUNT_NV = 0x909E

pattern GL_PATH_COVER_DEPTH_FUNC_NV  :: (Eq a, Num a) => a

pattern GL_PATH_COVER_DEPTH_FUNC_NV = 0x90BF

pattern GL_PATH_DASH_ARRAY_COUNT_NV  :: (Eq a, Num a) => a

pattern GL_PATH_DASH_ARRAY_COUNT_NV = 0x909F

pattern GL_PATH_DASH_CAPS_NV  :: (Eq a, Num a) => a

pattern GL_PATH_DASH_CAPS_NV = 0x907B

pattern GL_PATH_DASH_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_PATH_DASH_OFFSET_NV = 0x907E

pattern GL_PATH_DASH_OFFSET_RESET_NV  :: (Eq a, Num a) => a

pattern GL_PATH_DASH_OFFSET_RESET_NV = 0x90B4

pattern GL_PATH_END_CAPS_NV  :: (Eq a, Num a) => a

pattern GL_PATH_END_CAPS_NV = 0x9076

pattern GL_PATH_ERROR_POSITION_NV  :: (Eq a, Num a) => a

pattern GL_PATH_ERROR_POSITION_NV = 0x90AB

pattern GL_PATH_FILL_BOUNDING_BOX_NV  :: (Eq a, Num a) => a

pattern GL_PATH_FILL_BOUNDING_BOX_NV = 0x90A1

pattern GL_PATH_FILL_COVER_MODE_NV  :: (Eq a, Num a) => a

pattern GL_PATH_FILL_COVER_MODE_NV = 0x9082

pattern GL_PATH_FILL_MASK_NV  :: (Eq a, Num a) => a

pattern GL_PATH_FILL_MASK_NV = 0x9081

pattern GL_PATH_FILL_MODE_NV  :: (Eq a, Num a) => a

pattern GL_PATH_FILL_MODE_NV = 0x9080

pattern GL_PATH_FOG_GEN_MODE_NV  :: (Eq a, Num a) => a

pattern GL_PATH_FOG_GEN_MODE_NV = 0x90AC

pattern GL_PATH_FORMAT_PS_NV  :: (Eq a, Num a) => a

pattern GL_PATH_FORMAT_PS_NV = 0x9071

pattern GL_PATH_FORMAT_SVG_NV  :: (Eq a, Num a) => a

pattern GL_PATH_FORMAT_SVG_NV = 0x9070

pattern GL_PATH_GEN_COEFF_NV  :: (Eq a, Num a) => a

pattern GL_PATH_GEN_COEFF_NV = 0x90B1

pattern GL_PATH_GEN_COLOR_FORMAT_NV  :: (Eq a, Num a) => a

pattern GL_PATH_GEN_COLOR_FORMAT_NV = 0x90B2

pattern GL_PATH_GEN_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_PATH_GEN_COMPONENTS_NV = 0x90B3

pattern GL_PATH_GEN_MODE_NV  :: (Eq a, Num a) => a

pattern GL_PATH_GEN_MODE_NV = 0x90B0

pattern GL_PATH_INITIAL_DASH_CAP_NV  :: (Eq a, Num a) => a

pattern GL_PATH_INITIAL_DASH_CAP_NV = 0x907C

pattern GL_PATH_INITIAL_END_CAP_NV  :: (Eq a, Num a) => a

pattern GL_PATH_INITIAL_END_CAP_NV = 0x9077

pattern GL_PATH_JOIN_STYLE_NV  :: (Eq a, Num a) => a

pattern GL_PATH_JOIN_STYLE_NV = 0x9079

pattern GL_PATH_MAX_MODELVIEW_STACK_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_PATH_MAX_MODELVIEW_STACK_DEPTH_NV = 0x0D36

pattern GL_PATH_MAX_PROJECTION_STACK_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_PATH_MAX_PROJECTION_STACK_DEPTH_NV = 0x0D38

pattern GL_PATH_MITER_LIMIT_NV  :: (Eq a, Num a) => a

pattern GL_PATH_MITER_LIMIT_NV = 0x907A

pattern GL_PATH_MODELVIEW_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_PATH_MODELVIEW_MATRIX_NV = 0x0BA6

pattern GL_PATH_MODELVIEW_NV  :: (Eq a, Num a) => a

pattern GL_PATH_MODELVIEW_NV = 0x1700

pattern GL_PATH_MODELVIEW_STACK_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_PATH_MODELVIEW_STACK_DEPTH_NV = 0x0BA3

pattern GL_PATH_OBJECT_BOUNDING_BOX_NV  :: (Eq a, Num a) => a

pattern GL_PATH_OBJECT_BOUNDING_BOX_NV = 0x908A

pattern GL_PATH_PROJECTION_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_PATH_PROJECTION_MATRIX_NV = 0x0BA7

pattern GL_PATH_PROJECTION_NV  :: (Eq a, Num a) => a

pattern GL_PATH_PROJECTION_NV = 0x1701

pattern GL_PATH_PROJECTION_STACK_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_PATH_PROJECTION_STACK_DEPTH_NV = 0x0BA4

pattern GL_PATH_STENCIL_DEPTH_OFFSET_FACTOR_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STENCIL_DEPTH_OFFSET_FACTOR_NV = 0x90BD

pattern GL_PATH_STENCIL_DEPTH_OFFSET_UNITS_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STENCIL_DEPTH_OFFSET_UNITS_NV = 0x90BE

pattern GL_PATH_STENCIL_FUNC_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STENCIL_FUNC_NV = 0x90B7

pattern GL_PATH_STENCIL_REF_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STENCIL_REF_NV = 0x90B8

pattern GL_PATH_STENCIL_VALUE_MASK_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STENCIL_VALUE_MASK_NV = 0x90B9

pattern GL_PATH_STROKE_BOUNDING_BOX_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STROKE_BOUNDING_BOX_NV = 0x90A2

pattern GL_PATH_STROKE_COVER_MODE_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STROKE_COVER_MODE_NV = 0x9083

pattern GL_PATH_STROKE_MASK_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STROKE_MASK_NV = 0x9084

pattern GL_PATH_STROKE_WIDTH_NV  :: (Eq a, Num a) => a

pattern GL_PATH_STROKE_WIDTH_NV = 0x9075

pattern GL_PATH_TERMINAL_DASH_CAP_NV  :: (Eq a, Num a) => a

pattern GL_PATH_TERMINAL_DASH_CAP_NV = 0x907D

pattern GL_PATH_TERMINAL_END_CAP_NV  :: (Eq a, Num a) => a

pattern GL_PATH_TERMINAL_END_CAP_NV = 0x9078

pattern GL_PATH_TRANSPOSE_MODELVIEW_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_PATH_TRANSPOSE_MODELVIEW_MATRIX_NV = 0x84E3

pattern GL_PATH_TRANSPOSE_PROJECTION_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_PATH_TRANSPOSE_PROJECTION_MATRIX_NV = 0x84E4

pattern GL_QUADRATIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_QUADRATIC_CURVE_TO_NV = 0x0A

pattern GL_RECT_NV  :: (Eq a, Num a) => a

pattern GL_RECT_NV = 0xF6

pattern GL_RELATIVE_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_ARC_TO_NV = 0xFF

pattern GL_RELATIVE_CONIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_CONIC_CURVE_TO_NV = 0x1B

pattern GL_RELATIVE_CUBIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_CUBIC_CURVE_TO_NV = 0x0D

pattern GL_RELATIVE_HORIZONTAL_LINE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_HORIZONTAL_LINE_TO_NV = 0x07

pattern GL_RELATIVE_LARGE_CCW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_LARGE_CCW_ARC_TO_NV = 0x17

pattern GL_RELATIVE_LARGE_CW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_LARGE_CW_ARC_TO_NV = 0x19

pattern GL_RELATIVE_LINE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_LINE_TO_NV = 0x05

pattern GL_RELATIVE_MOVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_MOVE_TO_NV = 0x03

pattern GL_RELATIVE_QUADRATIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_QUADRATIC_CURVE_TO_NV = 0x0B

pattern GL_RELATIVE_RECT_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_RECT_NV = 0xF7

pattern GL_RELATIVE_ROUNDED_RECT2_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_ROUNDED_RECT2_NV = 0xEB

pattern GL_RELATIVE_ROUNDED_RECT4_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_ROUNDED_RECT4_NV = 0xED

pattern GL_RELATIVE_ROUNDED_RECT8_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_ROUNDED_RECT8_NV = 0xEF

pattern GL_RELATIVE_ROUNDED_RECT_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_ROUNDED_RECT_NV = 0xE9

pattern GL_RELATIVE_SMALL_CCW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_SMALL_CCW_ARC_TO_NV = 0x13

pattern GL_RELATIVE_SMALL_CW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_SMALL_CW_ARC_TO_NV = 0x15

pattern GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV = 0x11

pattern GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV = 0x0F

pattern GL_RELATIVE_VERTICAL_LINE_TO_NV  :: (Eq a, Num a) => a

pattern GL_RELATIVE_VERTICAL_LINE_TO_NV = 0x09

pattern GL_RESTART_PATH_NV  :: (Eq a, Num a) => a

pattern GL_RESTART_PATH_NV = 0xF0

pattern GL_ROUNDED_RECT2_NV  :: (Eq a, Num a) => a

pattern GL_ROUNDED_RECT2_NV = 0xEA

pattern GL_ROUNDED_RECT4_NV  :: (Eq a, Num a) => a

pattern GL_ROUNDED_RECT4_NV = 0xEC

pattern GL_ROUNDED_RECT8_NV  :: (Eq a, Num a) => a

pattern GL_ROUNDED_RECT8_NV = 0xEE

pattern GL_ROUNDED_RECT_NV  :: (Eq a, Num a) => a

pattern GL_ROUNDED_RECT_NV = 0xE8

pattern GL_ROUND_NV  :: (Eq a, Num a) => a

pattern GL_ROUND_NV = 0x90A4

pattern GL_SKIP_MISSING_GLYPH_NV  :: (Eq a, Num a) => a

pattern GL_SKIP_MISSING_GLYPH_NV = 0x90A9

pattern GL_SMALL_CCW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_SMALL_CCW_ARC_TO_NV = 0x12

pattern GL_SMALL_CW_ARC_TO_NV  :: (Eq a, Num a) => a

pattern GL_SMALL_CW_ARC_TO_NV = 0x14

pattern GL_SMOOTH_CUBIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_SMOOTH_CUBIC_CURVE_TO_NV = 0x10

pattern GL_SMOOTH_QUADRATIC_CURVE_TO_NV  :: (Eq a, Num a) => a

pattern GL_SMOOTH_QUADRATIC_CURVE_TO_NV = 0x0E

pattern GL_SQUARE_NV  :: (Eq a, Num a) => a

pattern GL_SQUARE_NV = 0x90A3

pattern GL_STANDARD_FONT_FORMAT_NV  :: (Eq a, Num a) => a

pattern GL_STANDARD_FONT_FORMAT_NV = 0x936C

pattern GL_STANDARD_FONT_NAME_NV  :: (Eq a, Num a) => a

pattern GL_STANDARD_FONT_NAME_NV = 0x9072

pattern GL_SYSTEM_FONT_NAME_NV  :: (Eq a, Num a) => a

pattern GL_SYSTEM_FONT_NAME_NV = 0x9073

pattern GL_TRANSLATE_2D_NV  :: (Eq a, Num a) => a

pattern GL_TRANSLATE_2D_NV = 0x9090

pattern GL_TRANSLATE_3D_NV  :: (Eq a, Num a) => a

pattern GL_TRANSLATE_3D_NV = 0x9091

pattern GL_TRANSLATE_X_NV  :: (Eq a, Num a) => a

pattern GL_TRANSLATE_X_NV = 0x908E

pattern GL_TRANSLATE_Y_NV  :: (Eq a, Num a) => a

pattern GL_TRANSLATE_Y_NV = 0x908F

pattern GL_TRANSPOSE_AFFINE_2D_NV  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_AFFINE_2D_NV = 0x9096

pattern GL_TRANSPOSE_AFFINE_3D_NV  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_AFFINE_3D_NV = 0x9098

pattern GL_TRIANGULAR_NV  :: (Eq a, Num a) => a

pattern GL_TRIANGULAR_NV = 0x90A5

pattern GL_USE_MISSING_GLYPH_NV  :: (Eq a, Num a) => a

pattern GL_USE_MISSING_GLYPH_NV = 0x90AA

pattern GL_UTF16_NV  :: (Eq a, Num a) => a

pattern GL_UTF16_NV = 0x909B

pattern GL_UTF8_NV  :: (Eq a, Num a) => a

pattern GL_UTF8_NV = 0x909A

pattern GL_VERTICAL_LINE_TO_NV  :: (Eq a, Num a) => a

pattern GL_VERTICAL_LINE_TO_NV = 0x08