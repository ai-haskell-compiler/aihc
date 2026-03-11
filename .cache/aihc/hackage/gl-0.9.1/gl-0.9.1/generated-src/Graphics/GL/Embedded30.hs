-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Embedded30 (
  -- * Graphics.GL.Embedded30
    module Graphics.GL.Embedded20
  , glBeginQuery
  , glBeginTransformFeedback
  , glBindBufferBase
  , glBindBufferRange
  , glBindSampler
  , glBindTransformFeedback
  , glBindVertexArray
  , glBlitFramebuffer
  , glClearBufferfi
  , glClearBufferfv
  , glClearBufferiv
  , glClearBufferuiv
  , glClientWaitSync
  , glCompressedTexImage3D
  , glCompressedTexSubImage3D
  , glCopyBufferSubData
  , glCopyTexSubImage3D
  , glDeleteQueries
  , glDeleteSamplers
  , glDeleteSync
  , glDeleteTransformFeedbacks
  , glDeleteVertexArrays
  , glDrawArraysInstanced
  , glDrawBuffers
  , glDrawElementsInstanced
  , glDrawRangeElements
  , glEndQuery
  , glEndTransformFeedback
  , glFenceSync
  , glFlushMappedBufferRange
  , glFramebufferTextureLayer
  , glGenQueries
  , glGenSamplers
  , glGenTransformFeedbacks
  , glGenVertexArrays
  , glGetActiveUniformBlockName
  , glGetActiveUniformBlockiv
  , glGetActiveUniformsiv
  , glGetBufferParameteri64v
  , glGetBufferPointerv
  , glGetFragDataLocation
  , glGetInteger64i_v
  , glGetInteger64v
  , glGetIntegeri_v
  , glGetInternalformativ
  , glGetProgramBinary
  , glGetQueryObjectuiv
  , glGetQueryiv
  , glGetSamplerParameterfv
  , glGetSamplerParameteriv
  , glGetStringi
  , glGetSynciv
  , glGetTransformFeedbackVarying
  , glGetUniformBlockIndex
  , glGetUniformIndices
  , glGetUniformuiv
  , glGetVertexAttribIiv
  , glGetVertexAttribIuiv
  , glInvalidateFramebuffer
  , glInvalidateSubFramebuffer
  , glIsQuery
  , glIsSampler
  , glIsSync
  , glIsTransformFeedback
  , glIsVertexArray
  , glMapBufferRange
  , glPauseTransformFeedback
  , glProgramBinary
  , glProgramParameteri
  , glReadBuffer
  , glRenderbufferStorageMultisample
  , glResumeTransformFeedback
  , glSamplerParameterf
  , glSamplerParameterfv
  , glSamplerParameteri
  , glSamplerParameteriv
  , glTexImage3D
  , glTexStorage2D
  , glTexStorage3D
  , glTexSubImage3D
  , glTransformFeedbackVaryings
  , glUniform1ui
  , glUniform1uiv
  , glUniform2ui
  , glUniform2uiv
  , glUniform3ui
  , glUniform3uiv
  , glUniform4ui
  , glUniform4uiv
  , glUniformBlockBinding
  , glUniformMatrix2x3fv
  , glUniformMatrix2x4fv
  , glUniformMatrix3x2fv
  , glUniformMatrix3x4fv
  , glUniformMatrix4x2fv
  , glUniformMatrix4x3fv
  , glUnmapBuffer
  , glVertexAttribDivisor
  , glVertexAttribI4i
  , glVertexAttribI4iv
  , glVertexAttribI4ui
  , glVertexAttribI4uiv
  , glVertexAttribIPointer
  , glWaitSync
  , pattern GL_ACTIVE_UNIFORM_BLOCKS
  , pattern GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH
  , pattern GL_ALREADY_SIGNALED
  , pattern GL_ANY_SAMPLES_PASSED
  , pattern GL_ANY_SAMPLES_PASSED_CONSERVATIVE
  , pattern GL_BLUE
  , pattern GL_BUFFER_ACCESS_FLAGS
  , pattern GL_BUFFER_MAPPED
  , pattern GL_BUFFER_MAP_LENGTH
  , pattern GL_BUFFER_MAP_OFFSET
  , pattern GL_BUFFER_MAP_POINTER
  , pattern GL_COLOR
  , pattern GL_COLOR_ATTACHMENT1
  , pattern GL_COLOR_ATTACHMENT10
  , pattern GL_COLOR_ATTACHMENT11
  , pattern GL_COLOR_ATTACHMENT12
  , pattern GL_COLOR_ATTACHMENT13
  , pattern GL_COLOR_ATTACHMENT14
  , pattern GL_COLOR_ATTACHMENT15
  , pattern GL_COLOR_ATTACHMENT16
  , pattern GL_COLOR_ATTACHMENT17
  , pattern GL_COLOR_ATTACHMENT18
  , pattern GL_COLOR_ATTACHMENT19
  , pattern GL_COLOR_ATTACHMENT2
  , pattern GL_COLOR_ATTACHMENT20
  , pattern GL_COLOR_ATTACHMENT21
  , pattern GL_COLOR_ATTACHMENT22
  , pattern GL_COLOR_ATTACHMENT23
  , pattern GL_COLOR_ATTACHMENT24
  , pattern GL_COLOR_ATTACHMENT25
  , pattern GL_COLOR_ATTACHMENT26
  , pattern GL_COLOR_ATTACHMENT27
  , pattern GL_COLOR_ATTACHMENT28
  , pattern GL_COLOR_ATTACHMENT29
  , pattern GL_COLOR_ATTACHMENT3
  , pattern GL_COLOR_ATTACHMENT30
  , pattern GL_COLOR_ATTACHMENT31
  , pattern GL_COLOR_ATTACHMENT4
  , pattern GL_COLOR_ATTACHMENT5
  , pattern GL_COLOR_ATTACHMENT6
  , pattern GL_COLOR_ATTACHMENT7
  , pattern GL_COLOR_ATTACHMENT8
  , pattern GL_COLOR_ATTACHMENT9
  , pattern GL_COMPARE_REF_TO_TEXTURE
  , pattern GL_COMPRESSED_R11_EAC
  , pattern GL_COMPRESSED_RG11_EAC
  , pattern GL_COMPRESSED_RGB8_ETC2
  , pattern GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
  , pattern GL_COMPRESSED_RGBA8_ETC2_EAC
  , pattern GL_COMPRESSED_SIGNED_R11_EAC
  , pattern GL_COMPRESSED_SIGNED_RG11_EAC
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
  , pattern GL_COMPRESSED_SRGB8_ETC2
  , pattern GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
  , pattern GL_CONDITION_SATISFIED
  , pattern GL_COPY_READ_BUFFER
  , pattern GL_COPY_READ_BUFFER_BINDING
  , pattern GL_COPY_WRITE_BUFFER
  , pattern GL_COPY_WRITE_BUFFER_BINDING
  , pattern GL_CURRENT_QUERY
  , pattern GL_DEPTH
  , pattern GL_DEPTH24_STENCIL8
  , pattern GL_DEPTH32F_STENCIL8
  , pattern GL_DEPTH_COMPONENT24
  , pattern GL_DEPTH_COMPONENT32F
  , pattern GL_DEPTH_STENCIL
  , pattern GL_DEPTH_STENCIL_ATTACHMENT
  , pattern GL_DRAW_BUFFER0
  , pattern GL_DRAW_BUFFER1
  , pattern GL_DRAW_BUFFER10
  , pattern GL_DRAW_BUFFER11
  , pattern GL_DRAW_BUFFER12
  , pattern GL_DRAW_BUFFER13
  , pattern GL_DRAW_BUFFER14
  , pattern GL_DRAW_BUFFER15
  , pattern GL_DRAW_BUFFER2
  , pattern GL_DRAW_BUFFER3
  , pattern GL_DRAW_BUFFER4
  , pattern GL_DRAW_BUFFER5
  , pattern GL_DRAW_BUFFER6
  , pattern GL_DRAW_BUFFER7
  , pattern GL_DRAW_BUFFER8
  , pattern GL_DRAW_BUFFER9
  , pattern GL_DRAW_FRAMEBUFFER
  , pattern GL_DRAW_FRAMEBUFFER_BINDING
  , pattern GL_DYNAMIC_COPY
  , pattern GL_DYNAMIC_READ
  , pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV
  , pattern GL_FLOAT_MAT2x3
  , pattern GL_FLOAT_MAT2x4
  , pattern GL_FLOAT_MAT3x2
  , pattern GL_FLOAT_MAT3x4
  , pattern GL_FLOAT_MAT4x2
  , pattern GL_FLOAT_MAT4x3
  , pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING
  , pattern GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
  , pattern GL_FRAMEBUFFER_DEFAULT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
  , pattern GL_FRAMEBUFFER_UNDEFINED
  , pattern GL_GREEN
  , pattern GL_HALF_FLOAT
  , pattern GL_INTERLEAVED_ATTRIBS
  , pattern GL_INT_2_10_10_10_REV
  , pattern GL_INT_SAMPLER_2D
  , pattern GL_INT_SAMPLER_2D_ARRAY
  , pattern GL_INT_SAMPLER_3D
  , pattern GL_INT_SAMPLER_CUBE
  , pattern GL_INVALID_INDEX
  , pattern GL_MAJOR_VERSION
  , pattern GL_MAP_FLUSH_EXPLICIT_BIT
  , pattern GL_MAP_INVALIDATE_BUFFER_BIT
  , pattern GL_MAP_INVALIDATE_RANGE_BIT
  , pattern GL_MAP_READ_BIT
  , pattern GL_MAP_UNSYNCHRONIZED_BIT
  , pattern GL_MAP_WRITE_BIT
  , pattern GL_MAX
  , pattern GL_MAX_3D_TEXTURE_SIZE
  , pattern GL_MAX_ARRAY_TEXTURE_LAYERS
  , pattern GL_MAX_COLOR_ATTACHMENTS
  , pattern GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMBINED_UNIFORM_BLOCKS
  , pattern GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS
  , pattern GL_MAX_DRAW_BUFFERS
  , pattern GL_MAX_ELEMENTS_INDICES
  , pattern GL_MAX_ELEMENTS_VERTICES
  , pattern GL_MAX_ELEMENT_INDEX
  , pattern GL_MAX_FRAGMENT_INPUT_COMPONENTS
  , pattern GL_MAX_FRAGMENT_UNIFORM_BLOCKS
  , pattern GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
  , pattern GL_MAX_PROGRAM_TEXEL_OFFSET
  , pattern GL_MAX_SAMPLES
  , pattern GL_MAX_SERVER_WAIT_TIMEOUT
  , pattern GL_MAX_TEXTURE_LOD_BIAS
  , pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
  , pattern GL_MAX_UNIFORM_BLOCK_SIZE
  , pattern GL_MAX_UNIFORM_BUFFER_BINDINGS
  , pattern GL_MAX_VARYING_COMPONENTS
  , pattern GL_MAX_VERTEX_OUTPUT_COMPONENTS
  , pattern GL_MAX_VERTEX_UNIFORM_BLOCKS
  , pattern GL_MAX_VERTEX_UNIFORM_COMPONENTS
  , pattern GL_MIN
  , pattern GL_MINOR_VERSION
  , pattern GL_MIN_PROGRAM_TEXEL_OFFSET
  , pattern GL_NUM_EXTENSIONS
  , pattern GL_NUM_PROGRAM_BINARY_FORMATS
  , pattern GL_NUM_SAMPLE_COUNTS
  , pattern GL_OBJECT_TYPE
  , pattern GL_PACK_ROW_LENGTH
  , pattern GL_PACK_SKIP_PIXELS
  , pattern GL_PACK_SKIP_ROWS
  , pattern GL_PIXEL_PACK_BUFFER
  , pattern GL_PIXEL_PACK_BUFFER_BINDING
  , pattern GL_PIXEL_UNPACK_BUFFER
  , pattern GL_PIXEL_UNPACK_BUFFER_BINDING
  , pattern GL_PRIMITIVE_RESTART_FIXED_INDEX
  , pattern GL_PROGRAM_BINARY_FORMATS
  , pattern GL_PROGRAM_BINARY_LENGTH
  , pattern GL_PROGRAM_BINARY_RETRIEVABLE_HINT
  , pattern GL_QUERY_RESULT
  , pattern GL_QUERY_RESULT_AVAILABLE
  , pattern GL_R11F_G11F_B10F
  , pattern GL_R16F
  , pattern GL_R16I
  , pattern GL_R16UI
  , pattern GL_R32F
  , pattern GL_R32I
  , pattern GL_R32UI
  , pattern GL_R8
  , pattern GL_R8I
  , pattern GL_R8UI
  , pattern GL_R8_SNORM
  , pattern GL_RASTERIZER_DISCARD
  , pattern GL_READ_BUFFER
  , pattern GL_READ_FRAMEBUFFER
  , pattern GL_READ_FRAMEBUFFER_BINDING
  , pattern GL_RED
  , pattern GL_RED_INTEGER
  , pattern GL_RENDERBUFFER_SAMPLES
  , pattern GL_RG
  , pattern GL_RG16F
  , pattern GL_RG16I
  , pattern GL_RG16UI
  , pattern GL_RG32F
  , pattern GL_RG32I
  , pattern GL_RG32UI
  , pattern GL_RG8
  , pattern GL_RG8I
  , pattern GL_RG8UI
  , pattern GL_RG8_SNORM
  , pattern GL_RGB10_A2
  , pattern GL_RGB10_A2UI
  , pattern GL_RGB16F
  , pattern GL_RGB16I
  , pattern GL_RGB16UI
  , pattern GL_RGB32F
  , pattern GL_RGB32I
  , pattern GL_RGB32UI
  , pattern GL_RGB8
  , pattern GL_RGB8I
  , pattern GL_RGB8UI
  , pattern GL_RGB8_SNORM
  , pattern GL_RGB9_E5
  , pattern GL_RGBA16F
  , pattern GL_RGBA16I
  , pattern GL_RGBA16UI
  , pattern GL_RGBA32F
  , pattern GL_RGBA32I
  , pattern GL_RGBA32UI
  , pattern GL_RGBA8
  , pattern GL_RGBA8I
  , pattern GL_RGBA8UI
  , pattern GL_RGBA8_SNORM
  , pattern GL_RGBA_INTEGER
  , pattern GL_RGB_INTEGER
  , pattern GL_RG_INTEGER
  , pattern GL_SAMPLER_2D_ARRAY
  , pattern GL_SAMPLER_2D_ARRAY_SHADOW
  , pattern GL_SAMPLER_2D_SHADOW
  , pattern GL_SAMPLER_3D
  , pattern GL_SAMPLER_BINDING
  , pattern GL_SAMPLER_CUBE_SHADOW
  , pattern GL_SEPARATE_ATTRIBS
  , pattern GL_SIGNALED
  , pattern GL_SIGNED_NORMALIZED
  , pattern GL_SRGB
  , pattern GL_SRGB8
  , pattern GL_SRGB8_ALPHA8
  , pattern GL_STATIC_COPY
  , pattern GL_STATIC_READ
  , pattern GL_STENCIL
  , pattern GL_STREAM_COPY
  , pattern GL_STREAM_READ
  , pattern GL_SYNC_CONDITION
  , pattern GL_SYNC_FENCE
  , pattern GL_SYNC_FLAGS
  , pattern GL_SYNC_FLUSH_COMMANDS_BIT
  , pattern GL_SYNC_GPU_COMMANDS_COMPLETE
  , pattern GL_SYNC_STATUS
  , pattern GL_TEXTURE_2D_ARRAY
  , pattern GL_TEXTURE_3D
  , pattern GL_TEXTURE_BASE_LEVEL
  , pattern GL_TEXTURE_BINDING_2D_ARRAY
  , pattern GL_TEXTURE_BINDING_3D
  , pattern GL_TEXTURE_COMPARE_FUNC
  , pattern GL_TEXTURE_COMPARE_MODE
  , pattern GL_TEXTURE_IMMUTABLE_FORMAT
  , pattern GL_TEXTURE_IMMUTABLE_LEVELS
  , pattern GL_TEXTURE_MAX_LEVEL
  , pattern GL_TEXTURE_MAX_LOD
  , pattern GL_TEXTURE_MIN_LOD
  , pattern GL_TEXTURE_SWIZZLE_A
  , pattern GL_TEXTURE_SWIZZLE_B
  , pattern GL_TEXTURE_SWIZZLE_G
  , pattern GL_TEXTURE_SWIZZLE_R
  , pattern GL_TEXTURE_WRAP_R
  , pattern GL_TIMEOUT_EXPIRED
  , pattern GL_TIMEOUT_IGNORED
  , pattern GL_TRANSFORM_FEEDBACK
  , pattern GL_TRANSFORM_FEEDBACK_ACTIVE
  , pattern GL_TRANSFORM_FEEDBACK_BINDING
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_START
  , pattern GL_TRANSFORM_FEEDBACK_PAUSED
  , pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
  , pattern GL_TRANSFORM_FEEDBACK_VARYINGS
  , pattern GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
  , pattern GL_UNIFORM_ARRAY_STRIDE
  , pattern GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS
  , pattern GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES
  , pattern GL_UNIFORM_BLOCK_BINDING
  , pattern GL_UNIFORM_BLOCK_DATA_SIZE
  , pattern GL_UNIFORM_BLOCK_INDEX
  , pattern GL_UNIFORM_BLOCK_NAME_LENGTH
  , pattern GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER
  , pattern GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER
  , pattern GL_UNIFORM_BUFFER
  , pattern GL_UNIFORM_BUFFER_BINDING
  , pattern GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT
  , pattern GL_UNIFORM_BUFFER_SIZE
  , pattern GL_UNIFORM_BUFFER_START
  , pattern GL_UNIFORM_IS_ROW_MAJOR
  , pattern GL_UNIFORM_MATRIX_STRIDE
  , pattern GL_UNIFORM_NAME_LENGTH
  , pattern GL_UNIFORM_OFFSET
  , pattern GL_UNIFORM_SIZE
  , pattern GL_UNIFORM_TYPE
  , pattern GL_UNPACK_IMAGE_HEIGHT
  , pattern GL_UNPACK_ROW_LENGTH
  , pattern GL_UNPACK_SKIP_IMAGES
  , pattern GL_UNPACK_SKIP_PIXELS
  , pattern GL_UNPACK_SKIP_ROWS
  , pattern GL_UNSIGNALED
  , pattern GL_UNSIGNED_INT_10F_11F_11F_REV
  , pattern GL_UNSIGNED_INT_24_8
  , pattern GL_UNSIGNED_INT_2_10_10_10_REV
  , pattern GL_UNSIGNED_INT_5_9_9_9_REV
  , pattern GL_UNSIGNED_INT_SAMPLER_2D
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
  , pattern GL_UNSIGNED_INT_SAMPLER_3D
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE
  , pattern GL_UNSIGNED_INT_VEC2
  , pattern GL_UNSIGNED_INT_VEC3
  , pattern GL_UNSIGNED_INT_VEC4
  , pattern GL_UNSIGNED_NORMALIZED
  , pattern GL_VERTEX_ARRAY_BINDING
  , pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR
  , pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER
  , pattern GL_WAIT_FAILED
) where

import Graphics.GL.Internal.Shared

import Graphics.GL.Embedded20