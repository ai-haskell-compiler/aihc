-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Embedded20 (
  -- * Graphics.GL.Embedded20
    glActiveTexture
  , glAttachShader
  , glBindAttribLocation
  , glBindBuffer
  , glBindFramebuffer
  , glBindRenderbuffer
  , glBindTexture
  , glBlendColor
  , glBlendEquation
  , glBlendEquationSeparate
  , glBlendFunc
  , glBlendFuncSeparate
  , glBufferData
  , glBufferSubData
  , glCheckFramebufferStatus
  , glClear
  , glClearColor
  , glClearDepthf
  , glClearStencil
  , glColorMask
  , glCompileShader
  , glCompressedTexImage2D
  , glCompressedTexSubImage2D
  , glCopyTexImage2D
  , glCopyTexSubImage2D
  , glCreateProgram
  , glCreateShader
  , glCullFace
  , glDeleteBuffers
  , glDeleteFramebuffers
  , glDeleteProgram
  , glDeleteRenderbuffers
  , glDeleteShader
  , glDeleteTextures
  , glDepthFunc
  , glDepthMask
  , glDepthRangef
  , glDetachShader
  , glDisable
  , glDisableVertexAttribArray
  , glDrawArrays
  , glDrawElements
  , glEnable
  , glEnableVertexAttribArray
  , glFinish
  , glFlush
  , glFramebufferRenderbuffer
  , glFramebufferTexture2D
  , glFrontFace
  , glGenBuffers
  , glGenFramebuffers
  , glGenRenderbuffers
  , glGenTextures
  , glGenerateMipmap
  , glGetActiveAttrib
  , glGetActiveUniform
  , glGetAttachedShaders
  , glGetAttribLocation
  , glGetBooleanv
  , glGetBufferParameteriv
  , glGetError
  , glGetFloatv
  , glGetFramebufferAttachmentParameteriv
  , glGetIntegerv
  , glGetProgramInfoLog
  , glGetProgramiv
  , glGetRenderbufferParameteriv
  , glGetShaderInfoLog
  , glGetShaderPrecisionFormat
  , glGetShaderSource
  , glGetShaderiv
  , glGetString
  , glGetTexParameterfv
  , glGetTexParameteriv
  , glGetUniformLocation
  , glGetUniformfv
  , glGetUniformiv
  , glGetVertexAttribPointerv
  , glGetVertexAttribfv
  , glGetVertexAttribiv
  , glHint
  , glIsBuffer
  , glIsEnabled
  , glIsFramebuffer
  , glIsProgram
  , glIsRenderbuffer
  , glIsShader
  , glIsTexture
  , glLineWidth
  , glLinkProgram
  , glPixelStorei
  , glPolygonOffset
  , glReadPixels
  , glReleaseShaderCompiler
  , glRenderbufferStorage
  , glSampleCoverage
  , glScissor
  , glShaderBinary
  , glShaderSource
  , glStencilFunc
  , glStencilFuncSeparate
  , glStencilMask
  , glStencilMaskSeparate
  , glStencilOp
  , glStencilOpSeparate
  , glTexImage2D
  , glTexParameterf
  , glTexParameterfv
  , glTexParameteri
  , glTexParameteriv
  , glTexSubImage2D
  , glUniform1f
  , glUniform1fv
  , glUniform1i
  , glUniform1iv
  , glUniform2f
  , glUniform2fv
  , glUniform2i
  , glUniform2iv
  , glUniform3f
  , glUniform3fv
  , glUniform3i
  , glUniform3iv
  , glUniform4f
  , glUniform4fv
  , glUniform4i
  , glUniform4iv
  , glUniformMatrix2fv
  , glUniformMatrix3fv
  , glUniformMatrix4fv
  , glUseProgram
  , glValidateProgram
  , glVertexAttrib1f
  , glVertexAttrib1fv
  , glVertexAttrib2f
  , glVertexAttrib2fv
  , glVertexAttrib3f
  , glVertexAttrib3fv
  , glVertexAttrib4f
  , glVertexAttrib4fv
  , glVertexAttribPointer
  , glViewport
  , pattern GL_ACTIVE_ATTRIBUTES
  , pattern GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
  , pattern GL_ACTIVE_TEXTURE
  , pattern GL_ACTIVE_UNIFORMS
  , pattern GL_ACTIVE_UNIFORM_MAX_LENGTH
  , pattern GL_ALIASED_LINE_WIDTH_RANGE
  , pattern GL_ALIASED_POINT_SIZE_RANGE
  , pattern GL_ALPHA
  , pattern GL_ALPHA_BITS
  , pattern GL_ALWAYS
  , pattern GL_ARRAY_BUFFER
  , pattern GL_ARRAY_BUFFER_BINDING
  , pattern GL_ATTACHED_SHADERS
  , pattern GL_BACK
  , pattern GL_BLEND
  , pattern GL_BLEND_COLOR
  , pattern GL_BLEND_DST_ALPHA
  , pattern GL_BLEND_DST_RGB
  , pattern GL_BLEND_EQUATION
  , pattern GL_BLEND_EQUATION_ALPHA
  , pattern GL_BLEND_EQUATION_RGB
  , pattern GL_BLEND_SRC_ALPHA
  , pattern GL_BLEND_SRC_RGB
  , pattern GL_BLUE_BITS
  , pattern GL_BOOL
  , pattern GL_BOOL_VEC2
  , pattern GL_BOOL_VEC3
  , pattern GL_BOOL_VEC4
  , pattern GL_BUFFER_SIZE
  , pattern GL_BUFFER_USAGE
  , pattern GL_BYTE
  , pattern GL_CCW
  , pattern GL_CLAMP_TO_EDGE
  , pattern GL_COLOR_ATTACHMENT0
  , pattern GL_COLOR_BUFFER_BIT
  , pattern GL_COLOR_CLEAR_VALUE
  , pattern GL_COLOR_WRITEMASK
  , pattern GL_COMPILE_STATUS
  , pattern GL_COMPRESSED_TEXTURE_FORMATS
  , pattern GL_CONSTANT_ALPHA
  , pattern GL_CONSTANT_COLOR
  , pattern GL_CULL_FACE
  , pattern GL_CULL_FACE_MODE
  , pattern GL_CURRENT_PROGRAM
  , pattern GL_CURRENT_VERTEX_ATTRIB
  , pattern GL_CW
  , pattern GL_DECR
  , pattern GL_DECR_WRAP
  , pattern GL_DELETE_STATUS
  , pattern GL_DEPTH_ATTACHMENT
  , pattern GL_DEPTH_BITS
  , pattern GL_DEPTH_BUFFER_BIT
  , pattern GL_DEPTH_CLEAR_VALUE
  , pattern GL_DEPTH_COMPONENT
  , pattern GL_DEPTH_COMPONENT16
  , pattern GL_DEPTH_FUNC
  , pattern GL_DEPTH_RANGE
  , pattern GL_DEPTH_TEST
  , pattern GL_DEPTH_WRITEMASK
  , pattern GL_DITHER
  , pattern GL_DONT_CARE
  , pattern GL_DST_ALPHA
  , pattern GL_DST_COLOR
  , pattern GL_DYNAMIC_DRAW
  , pattern GL_ELEMENT_ARRAY_BUFFER
  , pattern GL_ELEMENT_ARRAY_BUFFER_BINDING
  , pattern GL_EQUAL
  , pattern GL_EXTENSIONS
  , pattern GL_FALSE
  , pattern GL_FASTEST
  , pattern GL_FIXED
  , pattern GL_FLOAT
  , pattern GL_FLOAT_MAT2
  , pattern GL_FLOAT_MAT3
  , pattern GL_FLOAT_MAT4
  , pattern GL_FLOAT_VEC2
  , pattern GL_FLOAT_VEC3
  , pattern GL_FLOAT_VEC4
  , pattern GL_FRAGMENT_SHADER
  , pattern GL_FRAMEBUFFER
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
  , pattern GL_FRAMEBUFFER_BINDING
  , pattern GL_FRAMEBUFFER_COMPLETE
  , pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
  , pattern GL_FRAMEBUFFER_UNSUPPORTED
  , pattern GL_FRONT
  , pattern GL_FRONT_AND_BACK
  , pattern GL_FRONT_FACE
  , pattern GL_FUNC_ADD
  , pattern GL_FUNC_REVERSE_SUBTRACT
  , pattern GL_FUNC_SUBTRACT
  , pattern GL_GENERATE_MIPMAP_HINT
  , pattern GL_GEQUAL
  , pattern GL_GREATER
  , pattern GL_GREEN_BITS
  , pattern GL_HIGH_FLOAT
  , pattern GL_HIGH_INT
  , pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT
  , pattern GL_IMPLEMENTATION_COLOR_READ_TYPE
  , pattern GL_INCR
  , pattern GL_INCR_WRAP
  , pattern GL_INFO_LOG_LENGTH
  , pattern GL_INT
  , pattern GL_INT_VEC2
  , pattern GL_INT_VEC3
  , pattern GL_INT_VEC4
  , pattern GL_INVALID_ENUM
  , pattern GL_INVALID_FRAMEBUFFER_OPERATION
  , pattern GL_INVALID_OPERATION
  , pattern GL_INVALID_VALUE
  , pattern GL_INVERT
  , pattern GL_KEEP
  , pattern GL_LEQUAL
  , pattern GL_LESS
  , pattern GL_LINEAR
  , pattern GL_LINEAR_MIPMAP_LINEAR
  , pattern GL_LINEAR_MIPMAP_NEAREST
  , pattern GL_LINES
  , pattern GL_LINE_LOOP
  , pattern GL_LINE_STRIP
  , pattern GL_LINE_WIDTH
  , pattern GL_LINK_STATUS
  , pattern GL_LOW_FLOAT
  , pattern GL_LOW_INT
  , pattern GL_LUMINANCE
  , pattern GL_LUMINANCE_ALPHA
  , pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE
  , pattern GL_MAX_FRAGMENT_UNIFORM_VECTORS
  , pattern GL_MAX_RENDERBUFFER_SIZE
  , pattern GL_MAX_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TEXTURE_SIZE
  , pattern GL_MAX_VARYING_VECTORS
  , pattern GL_MAX_VERTEX_ATTRIBS
  , pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_VERTEX_UNIFORM_VECTORS
  , pattern GL_MAX_VIEWPORT_DIMS
  , pattern GL_MEDIUM_FLOAT
  , pattern GL_MEDIUM_INT
  , pattern GL_MIRRORED_REPEAT
  , pattern GL_NEAREST
  , pattern GL_NEAREST_MIPMAP_LINEAR
  , pattern GL_NEAREST_MIPMAP_NEAREST
  , pattern GL_NEVER
  , pattern GL_NICEST
  , pattern GL_NONE
  , pattern GL_NOTEQUAL
  , pattern GL_NO_ERROR
  , pattern GL_NUM_COMPRESSED_TEXTURE_FORMATS
  , pattern GL_NUM_SHADER_BINARY_FORMATS
  , pattern GL_ONE
  , pattern GL_ONE_MINUS_CONSTANT_ALPHA
  , pattern GL_ONE_MINUS_CONSTANT_COLOR
  , pattern GL_ONE_MINUS_DST_ALPHA
  , pattern GL_ONE_MINUS_DST_COLOR
  , pattern GL_ONE_MINUS_SRC_ALPHA
  , pattern GL_ONE_MINUS_SRC_COLOR
  , pattern GL_OUT_OF_MEMORY
  , pattern GL_PACK_ALIGNMENT
  , pattern GL_POINTS
  , pattern GL_POLYGON_OFFSET_FACTOR
  , pattern GL_POLYGON_OFFSET_FILL
  , pattern GL_POLYGON_OFFSET_UNITS
  , pattern GL_RED_BITS
  , pattern GL_RENDERBUFFER
  , pattern GL_RENDERBUFFER_ALPHA_SIZE
  , pattern GL_RENDERBUFFER_BINDING
  , pattern GL_RENDERBUFFER_BLUE_SIZE
  , pattern GL_RENDERBUFFER_DEPTH_SIZE
  , pattern GL_RENDERBUFFER_GREEN_SIZE
  , pattern GL_RENDERBUFFER_HEIGHT
  , pattern GL_RENDERBUFFER_INTERNAL_FORMAT
  , pattern GL_RENDERBUFFER_RED_SIZE
  , pattern GL_RENDERBUFFER_STENCIL_SIZE
  , pattern GL_RENDERBUFFER_WIDTH
  , pattern GL_RENDERER
  , pattern GL_REPEAT
  , pattern GL_REPLACE
  , pattern GL_RGB
  , pattern GL_RGB565
  , pattern GL_RGB5_A1
  , pattern GL_RGBA
  , pattern GL_RGBA4
  , pattern GL_SAMPLER_2D
  , pattern GL_SAMPLER_CUBE
  , pattern GL_SAMPLES
  , pattern GL_SAMPLE_ALPHA_TO_COVERAGE
  , pattern GL_SAMPLE_BUFFERS
  , pattern GL_SAMPLE_COVERAGE
  , pattern GL_SAMPLE_COVERAGE_INVERT
  , pattern GL_SAMPLE_COVERAGE_VALUE
  , pattern GL_SCISSOR_BOX
  , pattern GL_SCISSOR_TEST
  , pattern GL_SHADER_BINARY_FORMATS
  , pattern GL_SHADER_COMPILER
  , pattern GL_SHADER_SOURCE_LENGTH
  , pattern GL_SHADER_TYPE
  , pattern GL_SHADING_LANGUAGE_VERSION
  , pattern GL_SHORT
  , pattern GL_SRC_ALPHA
  , pattern GL_SRC_ALPHA_SATURATE
  , pattern GL_SRC_COLOR
  , pattern GL_STATIC_DRAW
  , pattern GL_STENCIL_ATTACHMENT
  , pattern GL_STENCIL_BACK_FAIL
  , pattern GL_STENCIL_BACK_FUNC
  , pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL
  , pattern GL_STENCIL_BACK_PASS_DEPTH_PASS
  , pattern GL_STENCIL_BACK_REF
  , pattern GL_STENCIL_BACK_VALUE_MASK
  , pattern GL_STENCIL_BACK_WRITEMASK
  , pattern GL_STENCIL_BITS
  , pattern GL_STENCIL_BUFFER_BIT
  , pattern GL_STENCIL_CLEAR_VALUE
  , pattern GL_STENCIL_FAIL
  , pattern GL_STENCIL_FUNC
  , pattern GL_STENCIL_INDEX8
  , pattern GL_STENCIL_PASS_DEPTH_FAIL
  , pattern GL_STENCIL_PASS_DEPTH_PASS
  , pattern GL_STENCIL_REF
  , pattern GL_STENCIL_TEST
  , pattern GL_STENCIL_VALUE_MASK
  , pattern GL_STENCIL_WRITEMASK
  , pattern GL_STREAM_DRAW
  , pattern GL_SUBPIXEL_BITS
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
  , pattern GL_TEXTURE_BINDING_CUBE_MAP
  , pattern GL_TEXTURE_CUBE_MAP
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  , pattern GL_TEXTURE_MAG_FILTER
  , pattern GL_TEXTURE_MIN_FILTER
  , pattern GL_TEXTURE_WRAP_S
  , pattern GL_TEXTURE_WRAP_T
  , pattern GL_TRIANGLES
  , pattern GL_TRIANGLE_FAN
  , pattern GL_TRIANGLE_STRIP
  , pattern GL_TRUE
  , pattern GL_UNPACK_ALIGNMENT
  , pattern GL_UNSIGNED_BYTE
  , pattern GL_UNSIGNED_INT
  , pattern GL_UNSIGNED_SHORT
  , pattern GL_UNSIGNED_SHORT_4_4_4_4
  , pattern GL_UNSIGNED_SHORT_5_5_5_1
  , pattern GL_UNSIGNED_SHORT_5_6_5
  , pattern GL_VALIDATE_STATUS
  , pattern GL_VENDOR
  , pattern GL_VERSION
  , pattern GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
  , pattern GL_VERTEX_ATTRIB_ARRAY_ENABLED
  , pattern GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
  , pattern GL_VERTEX_ATTRIB_ARRAY_POINTER
  , pattern GL_VERTEX_ATTRIB_ARRAY_SIZE
  , pattern GL_VERTEX_ATTRIB_ARRAY_STRIDE
  , pattern GL_VERTEX_ATTRIB_ARRAY_TYPE
  , pattern GL_VERTEX_SHADER
  , pattern GL_VIEWPORT
  , pattern GL_ZERO
) where

import Graphics.GL.Internal.Shared