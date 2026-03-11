-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Standard20 (
  -- * Graphics.GL.Standard20
    module Graphics.GL.Standard15
  , glAttachShader
  , glBindAttribLocation
  , glBlendEquationSeparate
  , glCompileShader
  , glCreateProgram
  , glCreateShader
  , glDeleteProgram
  , glDeleteShader
  , glDetachShader
  , glDisableVertexAttribArray
  , glDrawBuffers
  , glEnableVertexAttribArray
  , glGetActiveAttrib
  , glGetActiveUniform
  , glGetAttachedShaders
  , glGetAttribLocation
  , glGetProgramInfoLog
  , glGetProgramiv
  , glGetShaderInfoLog
  , glGetShaderSource
  , glGetShaderiv
  , glGetUniformLocation
  , glGetUniformfv
  , glGetUniformiv
  , glGetVertexAttribPointerv
  , glGetVertexAttribdv
  , glGetVertexAttribfv
  , glGetVertexAttribiv
  , glIsProgram
  , glIsShader
  , glLinkProgram
  , glShaderSource
  , glStencilFuncSeparate
  , glStencilMaskSeparate
  , glStencilOpSeparate
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
  , glVertexAttrib1d
  , glVertexAttrib1dv
  , glVertexAttrib1f
  , glVertexAttrib1fv
  , glVertexAttrib1s
  , glVertexAttrib1sv
  , glVertexAttrib2d
  , glVertexAttrib2dv
  , glVertexAttrib2f
  , glVertexAttrib2fv
  , glVertexAttrib2s
  , glVertexAttrib2sv
  , glVertexAttrib3d
  , glVertexAttrib3dv
  , glVertexAttrib3f
  , glVertexAttrib3fv
  , glVertexAttrib3s
  , glVertexAttrib3sv
  , glVertexAttrib4Nbv
  , glVertexAttrib4Niv
  , glVertexAttrib4Nsv
  , glVertexAttrib4Nub
  , glVertexAttrib4Nubv
  , glVertexAttrib4Nuiv
  , glVertexAttrib4Nusv
  , glVertexAttrib4bv
  , glVertexAttrib4d
  , glVertexAttrib4dv
  , glVertexAttrib4f
  , glVertexAttrib4fv
  , glVertexAttrib4iv
  , glVertexAttrib4s
  , glVertexAttrib4sv
  , glVertexAttrib4ubv
  , glVertexAttrib4uiv
  , glVertexAttrib4usv
  , glVertexAttribPointer
  , pattern GL_ACTIVE_ATTRIBUTES
  , pattern GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
  , pattern GL_ACTIVE_UNIFORMS
  , pattern GL_ACTIVE_UNIFORM_MAX_LENGTH
  , pattern GL_ATTACHED_SHADERS
  , pattern GL_BLEND_EQUATION_ALPHA
  , pattern GL_BLEND_EQUATION_RGB
  , pattern GL_BOOL
  , pattern GL_BOOL_VEC2
  , pattern GL_BOOL_VEC3
  , pattern GL_BOOL_VEC4
  , pattern GL_COMPILE_STATUS
  , pattern GL_COORD_REPLACE
  , pattern GL_CURRENT_PROGRAM
  , pattern GL_CURRENT_VERTEX_ATTRIB
  , pattern GL_DELETE_STATUS
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
  , pattern GL_FLOAT_MAT2
  , pattern GL_FLOAT_MAT3
  , pattern GL_FLOAT_MAT4
  , pattern GL_FLOAT_VEC2
  , pattern GL_FLOAT_VEC3
  , pattern GL_FLOAT_VEC4
  , pattern GL_FRAGMENT_SHADER
  , pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT
  , pattern GL_INFO_LOG_LENGTH
  , pattern GL_INT_VEC2
  , pattern GL_INT_VEC3
  , pattern GL_INT_VEC4
  , pattern GL_LINK_STATUS
  , pattern GL_LOWER_LEFT
  , pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_DRAW_BUFFERS
  , pattern GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
  , pattern GL_MAX_TEXTURE_COORDS
  , pattern GL_MAX_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_VARYING_FLOATS
  , pattern GL_MAX_VERTEX_ATTRIBS
  , pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_VERTEX_UNIFORM_COMPONENTS
  , pattern GL_POINT_SPRITE
  , pattern GL_POINT_SPRITE_COORD_ORIGIN
  , pattern GL_SAMPLER_1D
  , pattern GL_SAMPLER_1D_SHADOW
  , pattern GL_SAMPLER_2D
  , pattern GL_SAMPLER_2D_SHADOW
  , pattern GL_SAMPLER_3D
  , pattern GL_SAMPLER_CUBE
  , pattern GL_SHADER_SOURCE_LENGTH
  , pattern GL_SHADER_TYPE
  , pattern GL_SHADING_LANGUAGE_VERSION
  , pattern GL_STENCIL_BACK_FAIL
  , pattern GL_STENCIL_BACK_FUNC
  , pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL
  , pattern GL_STENCIL_BACK_PASS_DEPTH_PASS
  , pattern GL_STENCIL_BACK_REF
  , pattern GL_STENCIL_BACK_VALUE_MASK
  , pattern GL_STENCIL_BACK_WRITEMASK
  , pattern GL_UPPER_LEFT
  , pattern GL_VALIDATE_STATUS
  , pattern GL_VERTEX_ATTRIB_ARRAY_ENABLED
  , pattern GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
  , pattern GL_VERTEX_ATTRIB_ARRAY_POINTER
  , pattern GL_VERTEX_ATTRIB_ARRAY_SIZE
  , pattern GL_VERTEX_ATTRIB_ARRAY_STRIDE
  , pattern GL_VERTEX_ATTRIB_ARRAY_TYPE
  , pattern GL_VERTEX_PROGRAM_POINT_SIZE
  , pattern GL_VERTEX_PROGRAM_TWO_SIDE
  , pattern GL_VERTEX_SHADER
) where

import Graphics.GL.Internal.Shared

import Graphics.GL.Standard15