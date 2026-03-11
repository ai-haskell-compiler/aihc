-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Standard30 (
  -- * Graphics.GL.Standard30
    module Graphics.GL.Standard21
  , glBeginConditionalRender
  , glBeginTransformFeedback
  , glBindBufferBase
  , glBindBufferRange
  , glBindFragDataLocation
  , glBindFramebuffer
  , glBindRenderbuffer
  , glBindVertexArray
  , glBlitFramebuffer
  , glCheckFramebufferStatus
  , glClampColor
  , glClearBufferfi
  , glClearBufferfv
  , glClearBufferiv
  , glClearBufferuiv
  , glColorMaski
  , glDeleteFramebuffers
  , glDeleteRenderbuffers
  , glDeleteVertexArrays
  , glDisablei
  , glEnablei
  , glEndConditionalRender
  , glEndTransformFeedback
  , glFlushMappedBufferRange
  , glFramebufferRenderbuffer
  , glFramebufferTexture1D
  , glFramebufferTexture2D
  , glFramebufferTexture3D
  , glFramebufferTextureLayer
  , glGenFramebuffers
  , glGenRenderbuffers
  , glGenVertexArrays
  , glGenerateMipmap
  , glGetBooleani_v
  , glGetFragDataLocation
  , glGetFramebufferAttachmentParameteriv
  , glGetIntegeri_v
  , glGetRenderbufferParameteriv
  , glGetStringi
  , glGetTexParameterIiv
  , glGetTexParameterIuiv
  , glGetTransformFeedbackVarying
  , glGetUniformuiv
  , glGetVertexAttribIiv
  , glGetVertexAttribIuiv
  , glIsEnabledi
  , glIsFramebuffer
  , glIsRenderbuffer
  , glIsVertexArray
  , glMapBufferRange
  , glRenderbufferStorage
  , glRenderbufferStorageMultisample
  , glTexParameterIiv
  , glTexParameterIuiv
  , glTransformFeedbackVaryings
  , glUniform1ui
  , glUniform1uiv
  , glUniform2ui
  , glUniform2uiv
  , glUniform3ui
  , glUniform3uiv
  , glUniform4ui
  , glUniform4uiv
  , glVertexAttribI1i
  , glVertexAttribI1iv
  , glVertexAttribI1ui
  , glVertexAttribI1uiv
  , glVertexAttribI2i
  , glVertexAttribI2iv
  , glVertexAttribI2ui
  , glVertexAttribI2uiv
  , glVertexAttribI3i
  , glVertexAttribI3iv
  , glVertexAttribI3ui
  , glVertexAttribI3uiv
  , glVertexAttribI4bv
  , glVertexAttribI4i
  , glVertexAttribI4iv
  , glVertexAttribI4sv
  , glVertexAttribI4ubv
  , glVertexAttribI4ui
  , glVertexAttribI4uiv
  , glVertexAttribI4usv
  , glVertexAttribIPointer
  , pattern GL_ALPHA_INTEGER
  , pattern GL_BGRA_INTEGER
  , pattern GL_BGR_INTEGER
  , pattern GL_BLUE_INTEGER
  , pattern GL_BUFFER_ACCESS_FLAGS
  , pattern GL_BUFFER_MAP_LENGTH
  , pattern GL_BUFFER_MAP_OFFSET
  , pattern GL_CLAMP_FRAGMENT_COLOR
  , pattern GL_CLAMP_READ_COLOR
  , pattern GL_CLAMP_VERTEX_COLOR
  , pattern GL_CLIP_DISTANCE0
  , pattern GL_CLIP_DISTANCE1
  , pattern GL_CLIP_DISTANCE2
  , pattern GL_CLIP_DISTANCE3
  , pattern GL_CLIP_DISTANCE4
  , pattern GL_CLIP_DISTANCE5
  , pattern GL_CLIP_DISTANCE6
  , pattern GL_CLIP_DISTANCE7
  , pattern GL_COLOR_ATTACHMENT0
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
  , pattern GL_COMPRESSED_RED
  , pattern GL_COMPRESSED_RED_RGTC1
  , pattern GL_COMPRESSED_RG
  , pattern GL_COMPRESSED_RG_RGTC2
  , pattern GL_COMPRESSED_SIGNED_RED_RGTC1
  , pattern GL_COMPRESSED_SIGNED_RG_RGTC2
  , pattern GL_CONTEXT_FLAGS
  , pattern GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT
  , pattern GL_DEPTH24_STENCIL8
  , pattern GL_DEPTH32F_STENCIL8
  , pattern GL_DEPTH_ATTACHMENT
  , pattern GL_DEPTH_COMPONENT32F
  , pattern GL_DEPTH_STENCIL
  , pattern GL_DEPTH_STENCIL_ATTACHMENT
  , pattern GL_DRAW_FRAMEBUFFER
  , pattern GL_DRAW_FRAMEBUFFER_BINDING
  , pattern GL_FIXED_ONLY
  , pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV
  , pattern GL_FRAMEBUFFER
  , pattern GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING
  , pattern GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
  , pattern GL_FRAMEBUFFER_BINDING
  , pattern GL_FRAMEBUFFER_COMPLETE
  , pattern GL_FRAMEBUFFER_DEFAULT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
  , pattern GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER
  , pattern GL_FRAMEBUFFER_SRGB
  , pattern GL_FRAMEBUFFER_UNDEFINED
  , pattern GL_FRAMEBUFFER_UNSUPPORTED
  , pattern GL_GREEN_INTEGER
  , pattern GL_HALF_FLOAT
  , pattern GL_INDEX
  , pattern GL_INTERLEAVED_ATTRIBS
  , pattern GL_INT_SAMPLER_1D
  , pattern GL_INT_SAMPLER_1D_ARRAY
  , pattern GL_INT_SAMPLER_2D
  , pattern GL_INT_SAMPLER_2D_ARRAY
  , pattern GL_INT_SAMPLER_3D
  , pattern GL_INT_SAMPLER_CUBE
  , pattern GL_INVALID_FRAMEBUFFER_OPERATION
  , pattern GL_MAJOR_VERSION
  , pattern GL_MAP_FLUSH_EXPLICIT_BIT
  , pattern GL_MAP_INVALIDATE_BUFFER_BIT
  , pattern GL_MAP_INVALIDATE_RANGE_BIT
  , pattern GL_MAP_READ_BIT
  , pattern GL_MAP_UNSYNCHRONIZED_BIT
  , pattern GL_MAP_WRITE_BIT
  , pattern GL_MAX_ARRAY_TEXTURE_LAYERS
  , pattern GL_MAX_CLIP_DISTANCES
  , pattern GL_MAX_COLOR_ATTACHMENTS
  , pattern GL_MAX_PROGRAM_TEXEL_OFFSET
  , pattern GL_MAX_RENDERBUFFER_SIZE
  , pattern GL_MAX_SAMPLES
  , pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
  , pattern GL_MAX_VARYING_COMPONENTS
  , pattern GL_MINOR_VERSION
  , pattern GL_MIN_PROGRAM_TEXEL_OFFSET
  , pattern GL_NUM_EXTENSIONS
  , pattern GL_PRIMITIVES_GENERATED
  , pattern GL_PROXY_TEXTURE_1D_ARRAY
  , pattern GL_PROXY_TEXTURE_2D_ARRAY
  , pattern GL_QUERY_BY_REGION_NO_WAIT
  , pattern GL_QUERY_BY_REGION_WAIT
  , pattern GL_QUERY_NO_WAIT
  , pattern GL_QUERY_WAIT
  , pattern GL_R11F_G11F_B10F
  , pattern GL_R16
  , pattern GL_R16F
  , pattern GL_R16I
  , pattern GL_R16UI
  , pattern GL_R32F
  , pattern GL_R32I
  , pattern GL_R32UI
  , pattern GL_R8
  , pattern GL_R8I
  , pattern GL_R8UI
  , pattern GL_RASTERIZER_DISCARD
  , pattern GL_READ_FRAMEBUFFER
  , pattern GL_READ_FRAMEBUFFER_BINDING
  , pattern GL_RED_INTEGER
  , pattern GL_RENDERBUFFER
  , pattern GL_RENDERBUFFER_ALPHA_SIZE
  , pattern GL_RENDERBUFFER_BINDING
  , pattern GL_RENDERBUFFER_BLUE_SIZE
  , pattern GL_RENDERBUFFER_DEPTH_SIZE
  , pattern GL_RENDERBUFFER_GREEN_SIZE
  , pattern GL_RENDERBUFFER_HEIGHT
  , pattern GL_RENDERBUFFER_INTERNAL_FORMAT
  , pattern GL_RENDERBUFFER_RED_SIZE
  , pattern GL_RENDERBUFFER_SAMPLES
  , pattern GL_RENDERBUFFER_STENCIL_SIZE
  , pattern GL_RENDERBUFFER_WIDTH
  , pattern GL_RG
  , pattern GL_RG16
  , pattern GL_RG16F
  , pattern GL_RG16I
  , pattern GL_RG16UI
  , pattern GL_RG32F
  , pattern GL_RG32I
  , pattern GL_RG32UI
  , pattern GL_RG8
  , pattern GL_RG8I
  , pattern GL_RG8UI
  , pattern GL_RGB16F
  , pattern GL_RGB16I
  , pattern GL_RGB16UI
  , pattern GL_RGB32F
  , pattern GL_RGB32I
  , pattern GL_RGB32UI
  , pattern GL_RGB8I
  , pattern GL_RGB8UI
  , pattern GL_RGB9_E5
  , pattern GL_RGBA16F
  , pattern GL_RGBA16I
  , pattern GL_RGBA16UI
  , pattern GL_RGBA32F
  , pattern GL_RGBA32I
  , pattern GL_RGBA32UI
  , pattern GL_RGBA8I
  , pattern GL_RGBA8UI
  , pattern GL_RGBA_INTEGER
  , pattern GL_RGB_INTEGER
  , pattern GL_RG_INTEGER
  , pattern GL_SAMPLER_1D_ARRAY
  , pattern GL_SAMPLER_1D_ARRAY_SHADOW
  , pattern GL_SAMPLER_2D_ARRAY
  , pattern GL_SAMPLER_2D_ARRAY_SHADOW
  , pattern GL_SAMPLER_CUBE_SHADOW
  , pattern GL_SEPARATE_ATTRIBS
  , pattern GL_STENCIL_ATTACHMENT
  , pattern GL_STENCIL_INDEX1
  , pattern GL_STENCIL_INDEX16
  , pattern GL_STENCIL_INDEX4
  , pattern GL_STENCIL_INDEX8
  , pattern GL_TEXTURE_1D_ARRAY
  , pattern GL_TEXTURE_2D_ARRAY
  , pattern GL_TEXTURE_ALPHA_TYPE
  , pattern GL_TEXTURE_BINDING_1D_ARRAY
  , pattern GL_TEXTURE_BINDING_2D_ARRAY
  , pattern GL_TEXTURE_BLUE_TYPE
  , pattern GL_TEXTURE_DEPTH_TYPE
  , pattern GL_TEXTURE_GREEN_TYPE
  , pattern GL_TEXTURE_INTENSITY_TYPE
  , pattern GL_TEXTURE_LUMINANCE_TYPE
  , pattern GL_TEXTURE_RED_TYPE
  , pattern GL_TEXTURE_SHARED_SIZE
  , pattern GL_TEXTURE_STENCIL_SIZE
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_START
  , pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
  , pattern GL_TRANSFORM_FEEDBACK_VARYINGS
  , pattern GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
  , pattern GL_UNSIGNED_INT_10F_11F_11F_REV
  , pattern GL_UNSIGNED_INT_24_8
  , pattern GL_UNSIGNED_INT_5_9_9_9_REV
  , pattern GL_UNSIGNED_INT_SAMPLER_1D
  , pattern GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
  , pattern GL_UNSIGNED_INT_SAMPLER_2D
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
  , pattern GL_UNSIGNED_INT_SAMPLER_3D
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE
  , pattern GL_UNSIGNED_INT_VEC2
  , pattern GL_UNSIGNED_INT_VEC3
  , pattern GL_UNSIGNED_INT_VEC4
  , pattern GL_UNSIGNED_NORMALIZED
  , pattern GL_VERTEX_ARRAY_BINDING
  , pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER
) where

import Graphics.GL.Internal.Shared

import Graphics.GL.Standard21