-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Embedded32 (
  -- * Graphics.GL.Embedded32
    module Graphics.GL.Embedded31
  , glBlendBarrier
  , glBlendEquationSeparatei
  , glBlendEquationi
  , glBlendFuncSeparatei
  , glBlendFunci
  , glColorMaski
  , glCopyImageSubData
  , glDebugMessageCallback
  , glDebugMessageControl
  , glDebugMessageInsert
  , glDisablei
  , glDrawElementsBaseVertex
  , glDrawElementsInstancedBaseVertex
  , glDrawRangeElementsBaseVertex
  , glEnablei
  , glFramebufferTexture
  , glGetDebugMessageLog
  , glGetGraphicsResetStatus
  , glGetObjectLabel
  , glGetObjectPtrLabel
  , glGetPointerv
  , glGetSamplerParameterIiv
  , glGetSamplerParameterIuiv
  , glGetTexParameterIiv
  , glGetTexParameterIuiv
  , glGetnUniformfv
  , glGetnUniformiv
  , glGetnUniformuiv
  , glIsEnabledi
  , glMinSampleShading
  , glObjectLabel
  , glObjectPtrLabel
  , glPatchParameteri
  , glPopDebugGroup
  , glPrimitiveBoundingBox
  , glPushDebugGroup
  , glReadnPixels
  , glSamplerParameterIiv
  , glSamplerParameterIuiv
  , glTexBuffer
  , glTexBufferRange
  , glTexParameterIiv
  , glTexParameterIuiv
  , glTexStorage3DMultisample
  , pattern GL_BUFFER
  , pattern GL_CLAMP_TO_BORDER
  , pattern GL_COLORBURN
  , pattern GL_COLORDODGE
  , pattern GL_COMPRESSED_RGBA_ASTC_10x10
  , pattern GL_COMPRESSED_RGBA_ASTC_10x5
  , pattern GL_COMPRESSED_RGBA_ASTC_10x6
  , pattern GL_COMPRESSED_RGBA_ASTC_10x8
  , pattern GL_COMPRESSED_RGBA_ASTC_12x10
  , pattern GL_COMPRESSED_RGBA_ASTC_12x12
  , pattern GL_COMPRESSED_RGBA_ASTC_4x4
  , pattern GL_COMPRESSED_RGBA_ASTC_5x4
  , pattern GL_COMPRESSED_RGBA_ASTC_5x5
  , pattern GL_COMPRESSED_RGBA_ASTC_6x5
  , pattern GL_COMPRESSED_RGBA_ASTC_6x6
  , pattern GL_COMPRESSED_RGBA_ASTC_8x5
  , pattern GL_COMPRESSED_RGBA_ASTC_8x6
  , pattern GL_COMPRESSED_RGBA_ASTC_8x8
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8
  , pattern GL_CONTEXT_FLAGS
  , pattern GL_CONTEXT_FLAG_DEBUG_BIT
  , pattern GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT
  , pattern GL_CONTEXT_LOST
  , pattern GL_DARKEN
  , pattern GL_DEBUG_CALLBACK_FUNCTION
  , pattern GL_DEBUG_CALLBACK_USER_PARAM
  , pattern GL_DEBUG_GROUP_STACK_DEPTH
  , pattern GL_DEBUG_LOGGED_MESSAGES
  , pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH
  , pattern GL_DEBUG_OUTPUT
  , pattern GL_DEBUG_OUTPUT_SYNCHRONOUS
  , pattern GL_DEBUG_SEVERITY_HIGH
  , pattern GL_DEBUG_SEVERITY_LOW
  , pattern GL_DEBUG_SEVERITY_MEDIUM
  , pattern GL_DEBUG_SEVERITY_NOTIFICATION
  , pattern GL_DEBUG_SOURCE_API
  , pattern GL_DEBUG_SOURCE_APPLICATION
  , pattern GL_DEBUG_SOURCE_OTHER
  , pattern GL_DEBUG_SOURCE_SHADER_COMPILER
  , pattern GL_DEBUG_SOURCE_THIRD_PARTY
  , pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM
  , pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR
  , pattern GL_DEBUG_TYPE_ERROR
  , pattern GL_DEBUG_TYPE_MARKER
  , pattern GL_DEBUG_TYPE_OTHER
  , pattern GL_DEBUG_TYPE_PERFORMANCE
  , pattern GL_DEBUG_TYPE_POP_GROUP
  , pattern GL_DEBUG_TYPE_PORTABILITY
  , pattern GL_DEBUG_TYPE_PUSH_GROUP
  , pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR
  , pattern GL_DIFFERENCE
  , pattern GL_EXCLUSION
  , pattern GL_FIRST_VERTEX_CONVENTION
  , pattern GL_FRACTIONAL_EVEN
  , pattern GL_FRACTIONAL_ODD
  , pattern GL_FRAGMENT_INTERPOLATION_OFFSET_BITS
  , pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED
  , pattern GL_FRAMEBUFFER_DEFAULT_LAYERS
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
  , pattern GL_GEOMETRY_INPUT_TYPE
  , pattern GL_GEOMETRY_OUTPUT_TYPE
  , pattern GL_GEOMETRY_SHADER
  , pattern GL_GEOMETRY_SHADER_BIT
  , pattern GL_GEOMETRY_SHADER_INVOCATIONS
  , pattern GL_GEOMETRY_VERTICES_OUT
  , pattern GL_GUILTY_CONTEXT_RESET
  , pattern GL_HARDLIGHT
  , pattern GL_HSL_COLOR
  , pattern GL_HSL_HUE
  , pattern GL_HSL_LUMINOSITY
  , pattern GL_HSL_SATURATION
  , pattern GL_IMAGE_BUFFER
  , pattern GL_IMAGE_CUBE_MAP_ARRAY
  , pattern GL_INNOCENT_CONTEXT_RESET
  , pattern GL_INT_IMAGE_BUFFER
  , pattern GL_INT_IMAGE_CUBE_MAP_ARRAY
  , pattern GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
  , pattern GL_INT_SAMPLER_BUFFER
  , pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY
  , pattern GL_ISOLINES
  , pattern GL_IS_PER_PATCH
  , pattern GL_LAST_VERTEX_CONVENTION
  , pattern GL_LAYER_PROVOKING_VERTEX
  , pattern GL_LIGHTEN
  , pattern GL_LINES_ADJACENCY
  , pattern GL_LINE_STRIP_ADJACENCY
  , pattern GL_LOSE_CONTEXT_ON_RESET
  , pattern GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS
  , pattern GL_MAX_DEBUG_GROUP_STACK_DEPTH
  , pattern GL_MAX_DEBUG_LOGGED_MESSAGES
  , pattern GL_MAX_DEBUG_MESSAGE_LENGTH
  , pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET
  , pattern GL_MAX_FRAMEBUFFER_LAYERS
  , pattern GL_MAX_GEOMETRY_ATOMIC_COUNTERS
  , pattern GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_GEOMETRY_IMAGE_UNIFORMS
  , pattern GL_MAX_GEOMETRY_INPUT_COMPONENTS
  , pattern GL_MAX_GEOMETRY_OUTPUT_COMPONENTS
  , pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES
  , pattern GL_MAX_GEOMETRY_SHADER_INVOCATIONS
  , pattern GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS
  , pattern GL_MAX_GEOMETRY_UNIFORM_BLOCKS
  , pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS
  , pattern GL_MAX_LABEL_LENGTH
  , pattern GL_MAX_PATCH_VERTICES
  , pattern GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS
  , pattern GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS
  , pattern GL_MAX_TESS_CONTROL_INPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS
  , pattern GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS
  , pattern GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS
  , pattern GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS
  , pattern GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS
  , pattern GL_MAX_TESS_GEN_LEVEL
  , pattern GL_MAX_TESS_PATCH_COMPONENTS
  , pattern GL_MAX_TEXTURE_BUFFER_SIZE
  , pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
  , pattern GL_MIN_SAMPLE_SHADING_VALUE
  , pattern GL_MULTIPLY
  , pattern GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY
  , pattern GL_MULTISAMPLE_LINE_WIDTH_RANGE
  , pattern GL_NO_RESET_NOTIFICATION
  , pattern GL_OVERLAY
  , pattern GL_PATCHES
  , pattern GL_PATCH_VERTICES
  , pattern GL_PRIMITIVES_GENERATED
  , pattern GL_PRIMITIVE_BOUNDING_BOX
  , pattern GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED
  , pattern GL_PROGRAM
  , pattern GL_PROGRAM_PIPELINE
  , pattern GL_QUADS
  , pattern GL_QUERY
  , pattern GL_REFERENCED_BY_GEOMETRY_SHADER
  , pattern GL_REFERENCED_BY_TESS_CONTROL_SHADER
  , pattern GL_REFERENCED_BY_TESS_EVALUATION_SHADER
  , pattern GL_RESET_NOTIFICATION_STRATEGY
  , pattern GL_SAMPLER
  , pattern GL_SAMPLER_2D_MULTISAMPLE_ARRAY
  , pattern GL_SAMPLER_BUFFER
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW
  , pattern GL_SAMPLE_SHADING
  , pattern GL_SCREEN
  , pattern GL_SHADER
  , pattern GL_SOFTLIGHT
  , pattern GL_STACK_OVERFLOW
  , pattern GL_STACK_UNDERFLOW
  , pattern GL_TESS_CONTROL_OUTPUT_VERTICES
  , pattern GL_TESS_CONTROL_SHADER
  , pattern GL_TESS_CONTROL_SHADER_BIT
  , pattern GL_TESS_EVALUATION_SHADER
  , pattern GL_TESS_EVALUATION_SHADER_BIT
  , pattern GL_TESS_GEN_MODE
  , pattern GL_TESS_GEN_POINT_MODE
  , pattern GL_TESS_GEN_SPACING
  , pattern GL_TESS_GEN_VERTEX_ORDER
  , pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY
  , pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
  , pattern GL_TEXTURE_BINDING_BUFFER
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY
  , pattern GL_TEXTURE_BORDER_COLOR
  , pattern GL_TEXTURE_BUFFER
  , pattern GL_TEXTURE_BUFFER_BINDING
  , pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING
  , pattern GL_TEXTURE_BUFFER_OFFSET
  , pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT
  , pattern GL_TEXTURE_BUFFER_SIZE
  , pattern GL_TEXTURE_CUBE_MAP_ARRAY
  , pattern GL_TRIANGLES_ADJACENCY
  , pattern GL_TRIANGLE_STRIP_ADJACENCY
  , pattern GL_UNDEFINED_VERTEX
  , pattern GL_UNKNOWN_CONTEXT_RESET
  , pattern GL_UNSIGNED_INT_IMAGE_BUFFER
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
  , pattern GL_UNSIGNED_INT_SAMPLER_BUFFER
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY
  , pattern GL_VERTEX_ARRAY
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

import Graphics.GL.Embedded31

-- | Usage: @'glBlendBarrier'@


glBlendBarrier :: MonadIO m => m ()
glBlendBarrier = ffiIOV glBlendBarrierFunPtr

glBlendBarrierFunPtr :: FunPtr (IO ())
glBlendBarrierFunPtr = unsafePerformIO (getProcAddress "glBlendBarrier")

{-# NOINLINE glBlendBarrierFunPtr #-}

-- | Usage: @'glPrimitiveBoundingBox' minX minY minZ minW maxX maxY maxZ maxW@


glPrimitiveBoundingBox :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glPrimitiveBoundingBox = ffifloatfloatfloatfloatfloatfloatfloatfloatIOV glPrimitiveBoundingBoxFunPtr

glPrimitiveBoundingBoxFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glPrimitiveBoundingBoxFunPtr = unsafePerformIO (getProcAddress "glPrimitiveBoundingBox")

{-# NOINLINE glPrimitiveBoundingBoxFunPtr #-}

pattern GL_COLORBURN  :: (Eq a, Num a) => a

pattern GL_COLORBURN = 0x929A

pattern GL_COLORDODGE  :: (Eq a, Num a) => a

pattern GL_COLORDODGE = 0x9299

pattern GL_COMPRESSED_RGBA_ASTC_10x10  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_10x10 = 0x93BB

pattern GL_COMPRESSED_RGBA_ASTC_10x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_10x5 = 0x93B8

pattern GL_COMPRESSED_RGBA_ASTC_10x6  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_10x6 = 0x93B9

pattern GL_COMPRESSED_RGBA_ASTC_10x8  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_10x8 = 0x93BA

pattern GL_COMPRESSED_RGBA_ASTC_12x10  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_12x10 = 0x93BC

pattern GL_COMPRESSED_RGBA_ASTC_12x12  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_12x12 = 0x93BD

pattern GL_COMPRESSED_RGBA_ASTC_4x4  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_4x4 = 0x93B0

pattern GL_COMPRESSED_RGBA_ASTC_5x4  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_5x4 = 0x93B1

pattern GL_COMPRESSED_RGBA_ASTC_5x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_5x5 = 0x93B2

pattern GL_COMPRESSED_RGBA_ASTC_6x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_6x5 = 0x93B3

pattern GL_COMPRESSED_RGBA_ASTC_6x6  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_6x6 = 0x93B4

pattern GL_COMPRESSED_RGBA_ASTC_8x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_8x5 = 0x93B5

pattern GL_COMPRESSED_RGBA_ASTC_8x6  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_8x6 = 0x93B6

pattern GL_COMPRESSED_RGBA_ASTC_8x8  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_ASTC_8x8 = 0x93B7

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10 = 0x93DB

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5 = 0x93D8

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6 = 0x93D9

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8 = 0x93DA

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10 = 0x93DC

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12 = 0x93DD

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4 = 0x93D0

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4 = 0x93D1

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5 = 0x93D2

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5 = 0x93D3

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6 = 0x93D4

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5 = 0x93D5

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6 = 0x93D6

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8 = 0x93D7

pattern GL_DARKEN  :: (Eq a, Num a) => a

pattern GL_DARKEN = 0x9297

pattern GL_DIFFERENCE  :: (Eq a, Num a) => a

pattern GL_DIFFERENCE = 0x929E

pattern GL_EXCLUSION  :: (Eq a, Num a) => a

pattern GL_EXCLUSION = 0x92A0

pattern GL_HARDLIGHT  :: (Eq a, Num a) => a

pattern GL_HARDLIGHT = 0x929B

pattern GL_HSL_COLOR  :: (Eq a, Num a) => a

pattern GL_HSL_COLOR = 0x92AF

pattern GL_HSL_HUE  :: (Eq a, Num a) => a

pattern GL_HSL_HUE = 0x92AD

pattern GL_HSL_LUMINOSITY  :: (Eq a, Num a) => a

pattern GL_HSL_LUMINOSITY = 0x92B0

pattern GL_HSL_SATURATION  :: (Eq a, Num a) => a

pattern GL_HSL_SATURATION = 0x92AE

pattern GL_LIGHTEN  :: (Eq a, Num a) => a

pattern GL_LIGHTEN = 0x9298

pattern GL_MULTIPLY  :: (Eq a, Num a) => a

pattern GL_MULTIPLY = 0x9294

pattern GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY = 0x9382

pattern GL_MULTISAMPLE_LINE_WIDTH_RANGE  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_LINE_WIDTH_RANGE = 0x9381

pattern GL_OVERLAY  :: (Eq a, Num a) => a

pattern GL_OVERLAY = 0x9296

pattern GL_PRIMITIVE_BOUNDING_BOX  :: (Eq a, Num a) => a

pattern GL_PRIMITIVE_BOUNDING_BOX = 0x92BE

pattern GL_SCREEN  :: (Eq a, Num a) => a

pattern GL_SCREEN = 0x9295

pattern GL_SOFTLIGHT  :: (Eq a, Num a) => a

pattern GL_SOFTLIGHT = 0x929C