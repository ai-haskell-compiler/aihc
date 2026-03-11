-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.EmbeddedLite11 (
  -- * Graphics.GL.EmbeddedLite11
    glActiveTexture
  , glAlphaFuncx
  , glBindBuffer
  , glBindTexture
  , glBlendFunc
  , glBufferData
  , glBufferSubData
  , glClear
  , glClearColorx
  , glClearDepthx
  , glClearStencil
  , glClientActiveTexture
  , glClipPlanex
  , glColor4ub
  , glColor4x
  , glColorMask
  , glColorPointer
  , glCompressedTexImage2D
  , glCompressedTexSubImage2D
  , glCopyTexImage2D
  , glCopyTexSubImage2D
  , glCullFace
  , glDeleteBuffers
  , glDeleteTextures
  , glDepthFunc
  , glDepthMask
  , glDepthRangex
  , glDisable
  , glDisableClientState
  , glDrawArrays
  , glDrawElements
  , glEnable
  , glEnableClientState
  , glFinish
  , glFlush
  , glFogx
  , glFogxv
  , glFrontFace
  , glFrustumx
  , glGenBuffers
  , glGenTextures
  , glGetBooleanv
  , glGetBufferParameteriv
  , glGetClipPlanex
  , glGetError
  , glGetFixedv
  , glGetIntegerv
  , glGetLightxv
  , glGetMaterialxv
  , glGetPointerv
  , glGetString
  , glGetTexEnviv
  , glGetTexEnvxv
  , glGetTexParameteriv
  , glGetTexParameterxv
  , glHint
  , glIsBuffer
  , glIsEnabled
  , glIsTexture
  , glLightModelx
  , glLightModelxv
  , glLightx
  , glLightxv
  , glLineWidthx
  , glLoadIdentity
  , glLoadMatrixx
  , glLogicOp
  , glMaterialx
  , glMaterialxv
  , glMatrixMode
  , glMultMatrixx
  , glMultiTexCoord4x
  , glNormal3x
  , glNormalPointer
  , glOrthox
  , glPixelStorei
  , glPointParameterx
  , glPointParameterxv
  , glPointSizex
  , glPolygonOffsetx
  , glPopMatrix
  , glPushMatrix
  , glReadPixels
  , glRotatex
  , glSampleCoverage
  , glSampleCoveragex
  , glScalex
  , glScissor
  , glShadeModel
  , glStencilFunc
  , glStencilMask
  , glStencilOp
  , glTexCoordPointer
  , glTexEnvi
  , glTexEnviv
  , glTexEnvx
  , glTexEnvxv
  , glTexImage2D
  , glTexParameteri
  , glTexParameteriv
  , glTexParameterx
  , glTexParameterxv
  , glTexSubImage2D
  , glTranslatex
  , glVertexPointer
  , glViewport
  , pattern GL_ACTIVE_TEXTURE
  , pattern GL_ADD
  , pattern GL_ADD_SIGNED
  , pattern GL_ALIASED_LINE_WIDTH_RANGE
  , pattern GL_ALIASED_POINT_SIZE_RANGE
  , pattern GL_ALPHA
  , pattern GL_ALPHA_BITS
  , pattern GL_ALPHA_SCALE
  , pattern GL_ALPHA_TEST
  , pattern GL_ALPHA_TEST_FUNC
  , pattern GL_ALPHA_TEST_REF
  , pattern GL_ALWAYS
  , pattern GL_AMBIENT
  , pattern GL_AMBIENT_AND_DIFFUSE
  , pattern GL_AND
  , pattern GL_AND_INVERTED
  , pattern GL_AND_REVERSE
  , pattern GL_ARRAY_BUFFER
  , pattern GL_ARRAY_BUFFER_BINDING
  , pattern GL_BACK
  , pattern GL_BLEND
  , pattern GL_BLEND_DST
  , pattern GL_BLEND_SRC
  , pattern GL_BLUE_BITS
  , pattern GL_BUFFER_SIZE
  , pattern GL_BUFFER_USAGE
  , pattern GL_BYTE
  , pattern GL_CCW
  , pattern GL_CLAMP_TO_EDGE
  , pattern GL_CLEAR
  , pattern GL_CLIENT_ACTIVE_TEXTURE
  , pattern GL_CLIP_PLANE0
  , pattern GL_CLIP_PLANE1
  , pattern GL_CLIP_PLANE2
  , pattern GL_CLIP_PLANE3
  , pattern GL_CLIP_PLANE4
  , pattern GL_CLIP_PLANE5
  , pattern GL_COLOR_ARRAY
  , pattern GL_COLOR_ARRAY_BUFFER_BINDING
  , pattern GL_COLOR_ARRAY_POINTER
  , pattern GL_COLOR_ARRAY_SIZE
  , pattern GL_COLOR_ARRAY_STRIDE
  , pattern GL_COLOR_ARRAY_TYPE
  , pattern GL_COLOR_BUFFER_BIT
  , pattern GL_COLOR_CLEAR_VALUE
  , pattern GL_COLOR_LOGIC_OP
  , pattern GL_COLOR_MATERIAL
  , pattern GL_COLOR_WRITEMASK
  , pattern GL_COMBINE
  , pattern GL_COMBINE_ALPHA
  , pattern GL_COMBINE_RGB
  , pattern GL_COMPRESSED_TEXTURE_FORMATS
  , pattern GL_CONSTANT
  , pattern GL_CONSTANT_ATTENUATION
  , pattern GL_COPY
  , pattern GL_COPY_INVERTED
  , pattern GL_CULL_FACE
  , pattern GL_CULL_FACE_MODE
  , pattern GL_CURRENT_COLOR
  , pattern GL_CURRENT_NORMAL
  , pattern GL_CURRENT_TEXTURE_COORDS
  , pattern GL_CW
  , pattern GL_DECAL
  , pattern GL_DECR
  , pattern GL_DEPTH_BITS
  , pattern GL_DEPTH_BUFFER_BIT
  , pattern GL_DEPTH_CLEAR_VALUE
  , pattern GL_DEPTH_FUNC
  , pattern GL_DEPTH_RANGE
  , pattern GL_DEPTH_TEST
  , pattern GL_DEPTH_WRITEMASK
  , pattern GL_DIFFUSE
  , pattern GL_DITHER
  , pattern GL_DONT_CARE
  , pattern GL_DOT3_RGB
  , pattern GL_DOT3_RGBA
  , pattern GL_DST_ALPHA
  , pattern GL_DST_COLOR
  , pattern GL_DYNAMIC_DRAW
  , pattern GL_ELEMENT_ARRAY_BUFFER
  , pattern GL_ELEMENT_ARRAY_BUFFER_BINDING
  , pattern GL_EMISSION
  , pattern GL_EQUAL
  , pattern GL_EQUIV
  , pattern GL_EXP
  , pattern GL_EXP2
  , pattern GL_EXTENSIONS
  , pattern GL_FALSE
  , pattern GL_FASTEST
  , pattern GL_FIXED
  , pattern GL_FLAT
  , pattern GL_FLOAT
  , pattern GL_FOG
  , pattern GL_FOG_COLOR
  , pattern GL_FOG_DENSITY
  , pattern GL_FOG_END
  , pattern GL_FOG_HINT
  , pattern GL_FOG_MODE
  , pattern GL_FOG_START
  , pattern GL_FRONT
  , pattern GL_FRONT_AND_BACK
  , pattern GL_FRONT_FACE
  , pattern GL_GENERATE_MIPMAP
  , pattern GL_GENERATE_MIPMAP_HINT
  , pattern GL_GEQUAL
  , pattern GL_GREATER
  , pattern GL_GREEN_BITS
  , pattern GL_INCR
  , pattern GL_INTERPOLATE
  , pattern GL_INVALID_ENUM
  , pattern GL_INVALID_OPERATION
  , pattern GL_INVALID_VALUE
  , pattern GL_INVERT
  , pattern GL_KEEP
  , pattern GL_LEQUAL
  , pattern GL_LESS
  , pattern GL_LIGHT0
  , pattern GL_LIGHT1
  , pattern GL_LIGHT2
  , pattern GL_LIGHT3
  , pattern GL_LIGHT4
  , pattern GL_LIGHT5
  , pattern GL_LIGHT6
  , pattern GL_LIGHT7
  , pattern GL_LIGHTING
  , pattern GL_LIGHT_MODEL_AMBIENT
  , pattern GL_LIGHT_MODEL_TWO_SIDE
  , pattern GL_LINEAR
  , pattern GL_LINEAR_ATTENUATION
  , pattern GL_LINEAR_MIPMAP_LINEAR
  , pattern GL_LINEAR_MIPMAP_NEAREST
  , pattern GL_LINES
  , pattern GL_LINE_LOOP
  , pattern GL_LINE_SMOOTH
  , pattern GL_LINE_SMOOTH_HINT
  , pattern GL_LINE_STRIP
  , pattern GL_LINE_WIDTH
  , pattern GL_LOGIC_OP_MODE
  , pattern GL_LUMINANCE
  , pattern GL_LUMINANCE_ALPHA
  , pattern GL_MATRIX_MODE
  , pattern GL_MAX_CLIP_PLANES
  , pattern GL_MAX_LIGHTS
  , pattern GL_MAX_MODELVIEW_STACK_DEPTH
  , pattern GL_MAX_PROJECTION_STACK_DEPTH
  , pattern GL_MAX_TEXTURE_SIZE
  , pattern GL_MAX_TEXTURE_STACK_DEPTH
  , pattern GL_MAX_TEXTURE_UNITS
  , pattern GL_MAX_VIEWPORT_DIMS
  , pattern GL_MODELVIEW
  , pattern GL_MODELVIEW_MATRIX
  , pattern GL_MODELVIEW_STACK_DEPTH
  , pattern GL_MODULATE
  , pattern GL_MULTISAMPLE
  , pattern GL_NAND
  , pattern GL_NEAREST
  , pattern GL_NEAREST_MIPMAP_LINEAR
  , pattern GL_NEAREST_MIPMAP_NEAREST
  , pattern GL_NEVER
  , pattern GL_NICEST
  , pattern GL_NOOP
  , pattern GL_NOR
  , pattern GL_NORMALIZE
  , pattern GL_NORMAL_ARRAY
  , pattern GL_NORMAL_ARRAY_BUFFER_BINDING
  , pattern GL_NORMAL_ARRAY_POINTER
  , pattern GL_NORMAL_ARRAY_STRIDE
  , pattern GL_NORMAL_ARRAY_TYPE
  , pattern GL_NOTEQUAL
  , pattern GL_NO_ERROR
  , pattern GL_NUM_COMPRESSED_TEXTURE_FORMATS
  , pattern GL_ONE
  , pattern GL_ONE_MINUS_DST_ALPHA
  , pattern GL_ONE_MINUS_DST_COLOR
  , pattern GL_ONE_MINUS_SRC_ALPHA
  , pattern GL_ONE_MINUS_SRC_COLOR
  , pattern GL_OPERAND0_ALPHA
  , pattern GL_OPERAND0_RGB
  , pattern GL_OPERAND1_ALPHA
  , pattern GL_OPERAND1_RGB
  , pattern GL_OPERAND2_ALPHA
  , pattern GL_OPERAND2_RGB
  , pattern GL_OR
  , pattern GL_OR_INVERTED
  , pattern GL_OR_REVERSE
  , pattern GL_OUT_OF_MEMORY
  , pattern GL_PACK_ALIGNMENT
  , pattern GL_PERSPECTIVE_CORRECTION_HINT
  , pattern GL_POINTS
  , pattern GL_POINT_DISTANCE_ATTENUATION
  , pattern GL_POINT_FADE_THRESHOLD_SIZE
  , pattern GL_POINT_SIZE
  , pattern GL_POINT_SIZE_MAX
  , pattern GL_POINT_SIZE_MIN
  , pattern GL_POINT_SMOOTH
  , pattern GL_POINT_SMOOTH_HINT
  , pattern GL_POLYGON_OFFSET_FACTOR
  , pattern GL_POLYGON_OFFSET_FILL
  , pattern GL_POLYGON_OFFSET_UNITS
  , pattern GL_POSITION
  , pattern GL_PREVIOUS
  , pattern GL_PRIMARY_COLOR
  , pattern GL_PROJECTION
  , pattern GL_PROJECTION_MATRIX
  , pattern GL_PROJECTION_STACK_DEPTH
  , pattern GL_QUADRATIC_ATTENUATION
  , pattern GL_RED_BITS
  , pattern GL_RENDERER
  , pattern GL_REPEAT
  , pattern GL_REPLACE
  , pattern GL_RESCALE_NORMAL
  , pattern GL_RGB
  , pattern GL_RGBA
  , pattern GL_RGB_SCALE
  , pattern GL_SAMPLES
  , pattern GL_SAMPLE_ALPHA_TO_COVERAGE
  , pattern GL_SAMPLE_ALPHA_TO_ONE
  , pattern GL_SAMPLE_BUFFERS
  , pattern GL_SAMPLE_COVERAGE
  , pattern GL_SAMPLE_COVERAGE_INVERT
  , pattern GL_SAMPLE_COVERAGE_VALUE
  , pattern GL_SCISSOR_BOX
  , pattern GL_SCISSOR_TEST
  , pattern GL_SET
  , pattern GL_SHADE_MODEL
  , pattern GL_SHININESS
  , pattern GL_SHORT
  , pattern GL_SMOOTH
  , pattern GL_SMOOTH_LINE_WIDTH_RANGE
  , pattern GL_SMOOTH_POINT_SIZE_RANGE
  , pattern GL_SPECULAR
  , pattern GL_SPOT_CUTOFF
  , pattern GL_SPOT_DIRECTION
  , pattern GL_SPOT_EXPONENT
  , pattern GL_SRC0_ALPHA
  , pattern GL_SRC0_RGB
  , pattern GL_SRC1_ALPHA
  , pattern GL_SRC1_RGB
  , pattern GL_SRC2_ALPHA
  , pattern GL_SRC2_RGB
  , pattern GL_SRC_ALPHA
  , pattern GL_SRC_ALPHA_SATURATE
  , pattern GL_SRC_COLOR
  , pattern GL_STACK_OVERFLOW
  , pattern GL_STACK_UNDERFLOW
  , pattern GL_STATIC_DRAW
  , pattern GL_STENCIL_BITS
  , pattern GL_STENCIL_BUFFER_BIT
  , pattern GL_STENCIL_CLEAR_VALUE
  , pattern GL_STENCIL_FAIL
  , pattern GL_STENCIL_FUNC
  , pattern GL_STENCIL_PASS_DEPTH_FAIL
  , pattern GL_STENCIL_PASS_DEPTH_PASS
  , pattern GL_STENCIL_REF
  , pattern GL_STENCIL_TEST
  , pattern GL_STENCIL_VALUE_MASK
  , pattern GL_STENCIL_WRITEMASK
  , pattern GL_SUBPIXEL_BITS
  , pattern GL_SUBTRACT
  , pattern GL_TEXTURE
  , pattern GL_TEXTURE0
  , pattern GL_TEXTURE1
  , pattern GL_TEXTURE10
  , pattern GL_TEXTURE11
  , pattern GL_TEXTURE12
  , pattern GL_TEXTURE13
  , pattern GL_TEXTURE14
  , pattern GL_TEXTURE15
  , pattern GL_TEXTURE16
  , pattern GL_TEXTURE17
  , pattern GL_TEXTURE18
  , pattern GL_TEXTURE19
  , pattern GL_TEXTURE2
  , pattern GL_TEXTURE20
  , pattern GL_TEXTURE21
  , pattern GL_TEXTURE22
  , pattern GL_TEXTURE23
  , pattern GL_TEXTURE24
  , pattern GL_TEXTURE25
  , pattern GL_TEXTURE26
  , pattern GL_TEXTURE27
  , pattern GL_TEXTURE28
  , pattern GL_TEXTURE29
  , pattern GL_TEXTURE3
  , pattern GL_TEXTURE30
  , pattern GL_TEXTURE31
  , pattern GL_TEXTURE4
  , pattern GL_TEXTURE5
  , pattern GL_TEXTURE6
  , pattern GL_TEXTURE7
  , pattern GL_TEXTURE8
  , pattern GL_TEXTURE9
  , pattern GL_TEXTURE_2D
  , pattern GL_TEXTURE_BINDING_2D
  , pattern GL_TEXTURE_COORD_ARRAY
  , pattern GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING
  , pattern GL_TEXTURE_COORD_ARRAY_POINTER
  , pattern GL_TEXTURE_COORD_ARRAY_SIZE
  , pattern GL_TEXTURE_COORD_ARRAY_STRIDE
  , pattern GL_TEXTURE_COORD_ARRAY_TYPE
  , pattern GL_TEXTURE_ENV
  , pattern GL_TEXTURE_ENV_COLOR
  , pattern GL_TEXTURE_ENV_MODE
  , pattern GL_TEXTURE_MAG_FILTER
  , pattern GL_TEXTURE_MATRIX
  , pattern GL_TEXTURE_MIN_FILTER
  , pattern GL_TEXTURE_STACK_DEPTH
  , pattern GL_TEXTURE_WRAP_S
  , pattern GL_TEXTURE_WRAP_T
  , pattern GL_TRIANGLES
  , pattern GL_TRIANGLE_FAN
  , pattern GL_TRIANGLE_STRIP
  , pattern GL_TRUE
  , pattern GL_UNPACK_ALIGNMENT
  , pattern GL_UNSIGNED_BYTE
  , pattern GL_UNSIGNED_SHORT
  , pattern GL_UNSIGNED_SHORT_4_4_4_4
  , pattern GL_UNSIGNED_SHORT_5_5_5_1
  , pattern GL_UNSIGNED_SHORT_5_6_5
  , pattern GL_VENDOR
  , pattern GL_VERSION
  , pattern GL_VERSION_ES_CL_1_0
  , pattern GL_VERSION_ES_CL_1_1
  , pattern GL_VERSION_ES_CM_1_1
  , pattern GL_VERTEX_ARRAY
  , pattern GL_VERTEX_ARRAY_BUFFER_BINDING
  , pattern GL_VERTEX_ARRAY_POINTER
  , pattern GL_VERTEX_ARRAY_SIZE
  , pattern GL_VERTEX_ARRAY_STRIDE
  , pattern GL_VERTEX_ARRAY_TYPE
  , pattern GL_VIEWPORT
  , pattern GL_XOR
  , pattern GL_ZERO
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Usage: @'glAlphaFuncx' func ref@


glAlphaFuncx :: MonadIO m => GLenum -> GLfixed -> m ()
glAlphaFuncx = ffienumfixedIOV glAlphaFuncxFunPtr

glAlphaFuncxFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glAlphaFuncxFunPtr = unsafePerformIO (getProcAddress "glAlphaFuncx")

{-# NOINLINE glAlphaFuncxFunPtr #-}

-- | Usage: @'glClearColorx' red green blue alpha@


glClearColorx :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glClearColorx = ffifixedfixedfixedfixedIOV glClearColorxFunPtr

glClearColorxFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glClearColorxFunPtr = unsafePerformIO (getProcAddress "glClearColorx")

{-# NOINLINE glClearColorxFunPtr #-}

-- | Usage: @'glClearDepthx' depth@


glClearDepthx :: MonadIO m => GLfixed -> m ()
glClearDepthx = ffifixedIOV glClearDepthxFunPtr

glClearDepthxFunPtr :: FunPtr (GLfixed -> IO ())
glClearDepthxFunPtr = unsafePerformIO (getProcAddress "glClearDepthx")

{-# NOINLINE glClearDepthxFunPtr #-}

-- | Usage: @'glClipPlanex' plane equation@
--
-- The length of @equation@ should be @4@.


glClipPlanex :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glClipPlanex = ffienumPtrfixedIOV glClipPlanexFunPtr

glClipPlanexFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glClipPlanexFunPtr = unsafePerformIO (getProcAddress "glClipPlanex")

{-# NOINLINE glClipPlanexFunPtr #-}

-- | Usage: @'glColor4x' red green blue alpha@


glColor4x :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glColor4x = ffifixedfixedfixedfixedIOV glColor4xFunPtr

glColor4xFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glColor4xFunPtr = unsafePerformIO (getProcAddress "glColor4x")

{-# NOINLINE glColor4xFunPtr #-}

-- | Usage: @'glDepthRangex' n f@


glDepthRangex :: MonadIO m => GLfixed -> GLfixed -> m ()
glDepthRangex = ffifixedfixedIOV glDepthRangexFunPtr

glDepthRangexFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glDepthRangexFunPtr = unsafePerformIO (getProcAddress "glDepthRangex")

{-# NOINLINE glDepthRangexFunPtr #-}

-- | Usage: @'glFogx' pname param@


glFogx :: MonadIO m => GLenum -> GLfixed -> m ()
glFogx = ffienumfixedIOV glFogxFunPtr

glFogxFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glFogxFunPtr = unsafePerformIO (getProcAddress "glFogx")

{-# NOINLINE glFogxFunPtr #-}

-- | Usage: @'glFogxv' pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glFogxv :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glFogxv = ffienumPtrfixedIOV glFogxvFunPtr

glFogxvFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glFogxvFunPtr = unsafePerformIO (getProcAddress "glFogxv")

{-# NOINLINE glFogxvFunPtr #-}

-- | Usage: @'glFrustumx' l r b t n f@


glFrustumx :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glFrustumx = ffifixedfixedfixedfixedfixedfixedIOV glFrustumxFunPtr

glFrustumxFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glFrustumxFunPtr = unsafePerformIO (getProcAddress "glFrustumx")

{-# NOINLINE glFrustumxFunPtr #-}

-- | Usage: @'glGetClipPlanex' plane equation@
--
-- The length of @equation@ should be @4@.


glGetClipPlanex :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glGetClipPlanex = ffienumPtrfixedIOV glGetClipPlanexFunPtr

glGetClipPlanexFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glGetClipPlanexFunPtr = unsafePerformIO (getProcAddress "glGetClipPlanex")

{-# NOINLINE glGetClipPlanexFunPtr #-}

-- | Usage: @'glGetFixedv' pname params@


glGetFixedv :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glGetFixedv = ffienumPtrfixedIOV glGetFixedvFunPtr

glGetFixedvFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glGetFixedvFunPtr = unsafePerformIO (getProcAddress "glGetFixedv")

{-# NOINLINE glGetFixedvFunPtr #-}

-- | Usage: @'glGetLightxv' light pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetLightxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetLightxv = ffienumenumPtrfixedIOV glGetLightxvFunPtr

glGetLightxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetLightxvFunPtr = unsafePerformIO (getProcAddress "glGetLightxv")

{-# NOINLINE glGetLightxvFunPtr #-}

-- | Usage: @'glGetMaterialxv' face pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMaterialxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetMaterialxv = ffienumenumPtrfixedIOV glGetMaterialxvFunPtr

glGetMaterialxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetMaterialxvFunPtr = unsafePerformIO (getProcAddress "glGetMaterialxv")

{-# NOINLINE glGetMaterialxvFunPtr #-}

-- | Usage: @'glGetTexEnvxv' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTexEnvxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetTexEnvxv = ffienumenumPtrfixedIOV glGetTexEnvxvFunPtr

glGetTexEnvxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetTexEnvxvFunPtr = unsafePerformIO (getProcAddress "glGetTexEnvxv")

{-# NOINLINE glGetTexEnvxvFunPtr #-}

-- | Usage: @'glGetTexParameterxv' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTexParameterxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glGetTexParameterxv = ffienumenumPtrfixedIOV glGetTexParameterxvFunPtr

glGetTexParameterxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glGetTexParameterxvFunPtr = unsafePerformIO (getProcAddress "glGetTexParameterxv")

{-# NOINLINE glGetTexParameterxvFunPtr #-}

-- | Usage: @'glLightModelx' pname param@


glLightModelx :: MonadIO m => GLenum -> GLfixed -> m ()
glLightModelx = ffienumfixedIOV glLightModelxFunPtr

glLightModelxFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glLightModelxFunPtr = unsafePerformIO (getProcAddress "glLightModelx")

{-# NOINLINE glLightModelxFunPtr #-}

-- | Usage: @'glLightModelxv' pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glLightModelxv :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glLightModelxv = ffienumPtrfixedIOV glLightModelxvFunPtr

glLightModelxvFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glLightModelxvFunPtr = unsafePerformIO (getProcAddress "glLightModelxv")

{-# NOINLINE glLightModelxvFunPtr #-}

-- | Usage: @'glLightx' light pname param@


glLightx :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glLightx = ffienumenumfixedIOV glLightxFunPtr

glLightxFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glLightxFunPtr = unsafePerformIO (getProcAddress "glLightx")

{-# NOINLINE glLightxFunPtr #-}

-- | Usage: @'glLightxv' light pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glLightxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glLightxv = ffienumenumPtrfixedIOV glLightxvFunPtr

glLightxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glLightxvFunPtr = unsafePerformIO (getProcAddress "glLightxv")

{-# NOINLINE glLightxvFunPtr #-}

-- | Usage: @'glLineWidthx' width@


glLineWidthx :: MonadIO m => GLfixed -> m ()
glLineWidthx = ffifixedIOV glLineWidthxFunPtr

glLineWidthxFunPtr :: FunPtr (GLfixed -> IO ())
glLineWidthxFunPtr = unsafePerformIO (getProcAddress "glLineWidthx")

{-# NOINLINE glLineWidthxFunPtr #-}

-- | Usage: @'glLoadMatrixx' m@
--
-- The length of @m@ should be @16@.


glLoadMatrixx :: MonadIO m => Ptr GLfixed -> m ()
glLoadMatrixx = ffiPtrfixedIOV glLoadMatrixxFunPtr

glLoadMatrixxFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glLoadMatrixxFunPtr = unsafePerformIO (getProcAddress "glLoadMatrixx")

{-# NOINLINE glLoadMatrixxFunPtr #-}

-- | Usage: @'glMaterialx' face pname param@


glMaterialx :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glMaterialx = ffienumenumfixedIOV glMaterialxFunPtr

glMaterialxFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glMaterialxFunPtr = unsafePerformIO (getProcAddress "glMaterialx")

{-# NOINLINE glMaterialxFunPtr #-}

-- | Usage: @'glMaterialxv' face pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glMaterialxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glMaterialxv = ffienumenumPtrfixedIOV glMaterialxvFunPtr

glMaterialxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glMaterialxvFunPtr = unsafePerformIO (getProcAddress "glMaterialxv")

{-# NOINLINE glMaterialxvFunPtr #-}

-- | Usage: @'glMultMatrixx' m@
--
-- The length of @m@ should be @16@.


glMultMatrixx :: MonadIO m => Ptr GLfixed -> m ()
glMultMatrixx = ffiPtrfixedIOV glMultMatrixxFunPtr

glMultMatrixxFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glMultMatrixxFunPtr = unsafePerformIO (getProcAddress "glMultMatrixx")

{-# NOINLINE glMultMatrixxFunPtr #-}

-- | Usage: @'glMultiTexCoord4x' texture s t r q@


glMultiTexCoord4x :: MonadIO m => GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glMultiTexCoord4x = ffienumfixedfixedfixedfixedIOV glMultiTexCoord4xFunPtr

glMultiTexCoord4xFunPtr :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glMultiTexCoord4xFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4x")

{-# NOINLINE glMultiTexCoord4xFunPtr #-}

-- | Usage: @'glNormal3x' nx ny nz@


glNormal3x :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glNormal3x = ffifixedfixedfixedIOV glNormal3xFunPtr

glNormal3xFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glNormal3xFunPtr = unsafePerformIO (getProcAddress "glNormal3x")

{-# NOINLINE glNormal3xFunPtr #-}

-- | Usage: @'glOrthox' l r b t n f@


glOrthox :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glOrthox = ffifixedfixedfixedfixedfixedfixedIOV glOrthoxFunPtr

glOrthoxFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glOrthoxFunPtr = unsafePerformIO (getProcAddress "glOrthox")

{-# NOINLINE glOrthoxFunPtr #-}

-- | Usage: @'glPointParameterx' pname param@


glPointParameterx :: MonadIO m => GLenum -> GLfixed -> m ()
glPointParameterx = ffienumfixedIOV glPointParameterxFunPtr

glPointParameterxFunPtr :: FunPtr (GLenum -> GLfixed -> IO ())
glPointParameterxFunPtr = unsafePerformIO (getProcAddress "glPointParameterx")

{-# NOINLINE glPointParameterxFunPtr #-}

-- | Usage: @'glPointParameterxv' pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glPointParameterxv :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glPointParameterxv = ffienumPtrfixedIOV glPointParameterxvFunPtr

glPointParameterxvFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glPointParameterxvFunPtr = unsafePerformIO (getProcAddress "glPointParameterxv")

{-# NOINLINE glPointParameterxvFunPtr #-}

-- | Usage: @'glPointSizex' size@


glPointSizex :: MonadIO m => GLfixed -> m ()
glPointSizex = ffifixedIOV glPointSizexFunPtr

glPointSizexFunPtr :: FunPtr (GLfixed -> IO ())
glPointSizexFunPtr = unsafePerformIO (getProcAddress "glPointSizex")

{-# NOINLINE glPointSizexFunPtr #-}

-- | Usage: @'glPolygonOffsetx' factor units@


glPolygonOffsetx :: MonadIO m => GLfixed -> GLfixed -> m ()
glPolygonOffsetx = ffifixedfixedIOV glPolygonOffsetxFunPtr

glPolygonOffsetxFunPtr :: FunPtr (GLfixed -> GLfixed -> IO ())
glPolygonOffsetxFunPtr = unsafePerformIO (getProcAddress "glPolygonOffsetx")

{-# NOINLINE glPolygonOffsetxFunPtr #-}

-- | Usage: @'glRotatex' angle x y z@


glRotatex :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glRotatex = ffifixedfixedfixedfixedIOV glRotatexFunPtr

glRotatexFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glRotatexFunPtr = unsafePerformIO (getProcAddress "glRotatex")

{-# NOINLINE glRotatexFunPtr #-}

-- | Usage: @'glSampleCoveragex' value invert@


glSampleCoveragex :: MonadIO m => GLclampx -> GLboolean -> m ()
glSampleCoveragex = fficlampxbooleanIOV glSampleCoveragexFunPtr

glSampleCoveragexFunPtr :: FunPtr (GLclampx -> GLboolean -> IO ())
glSampleCoveragexFunPtr = unsafePerformIO (getProcAddress "glSampleCoveragex")

{-# NOINLINE glSampleCoveragexFunPtr #-}

-- | Usage: @'glScalex' x y z@


glScalex :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glScalex = ffifixedfixedfixedIOV glScalexFunPtr

glScalexFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glScalexFunPtr = unsafePerformIO (getProcAddress "glScalex")

{-# NOINLINE glScalexFunPtr #-}

-- | Usage: @'glTexEnvx' target pname param@


glTexEnvx :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glTexEnvx = ffienumenumfixedIOV glTexEnvxFunPtr

glTexEnvxFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glTexEnvxFunPtr = unsafePerformIO (getProcAddress "glTexEnvx")

{-# NOINLINE glTexEnvxFunPtr #-}

-- | Usage: @'glTexEnvxv' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTexEnvxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glTexEnvxv = ffienumenumPtrfixedIOV glTexEnvxvFunPtr

glTexEnvxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glTexEnvxvFunPtr = unsafePerformIO (getProcAddress "glTexEnvxv")

{-# NOINLINE glTexEnvxvFunPtr #-}

-- | Usage: @'glTexParameterx' target pname param@


glTexParameterx :: MonadIO m => GLenum -> GLenum -> GLfixed -> m ()
glTexParameterx = ffienumenumfixedIOV glTexParameterxFunPtr

glTexParameterxFunPtr :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
glTexParameterxFunPtr = unsafePerformIO (getProcAddress "glTexParameterx")

{-# NOINLINE glTexParameterxFunPtr #-}

-- | Usage: @'glTexParameterxv' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTexParameterxv :: MonadIO m => GLenum -> GLenum -> Ptr GLfixed -> m ()
glTexParameterxv = ffienumenumPtrfixedIOV glTexParameterxvFunPtr

glTexParameterxvFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
glTexParameterxvFunPtr = unsafePerformIO (getProcAddress "glTexParameterxv")

{-# NOINLINE glTexParameterxvFunPtr #-}

-- | Usage: @'glTranslatex' x y z@


glTranslatex :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> m ()
glTranslatex = ffifixedfixedfixedIOV glTranslatexFunPtr

glTranslatexFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
glTranslatexFunPtr = unsafePerformIO (getProcAddress "glTranslatex")

{-# NOINLINE glTranslatexFunPtr #-}

pattern GL_VERSION_ES_CL_1_0  :: (Eq a, Num a) => a

pattern GL_VERSION_ES_CL_1_0 = 1

pattern GL_VERSION_ES_CL_1_1  :: (Eq a, Num a) => a

pattern GL_VERSION_ES_CL_1_1 = 1

pattern GL_VERSION_ES_CM_1_1  :: (Eq a, Num a) => a

pattern GL_VERSION_ES_CM_1_1 = 1