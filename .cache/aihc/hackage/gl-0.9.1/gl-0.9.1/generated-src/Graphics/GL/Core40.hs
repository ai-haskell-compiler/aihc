-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Core40 (
  -- * Graphics.GL.Core40
    module Graphics.GL.Core33
  , glBeginQueryIndexed
  , glBindTransformFeedback
  , glBlendEquationSeparatei
  , glBlendEquationi
  , glBlendFuncSeparatei
  , glBlendFunci
  , glDeleteTransformFeedbacks
  , glDrawArraysIndirect
  , glDrawElementsIndirect
  , glDrawTransformFeedback
  , glDrawTransformFeedbackStream
  , glEndQueryIndexed
  , glGenTransformFeedbacks
  , glGetActiveSubroutineName
  , glGetActiveSubroutineUniformName
  , glGetActiveSubroutineUniformiv
  , glGetProgramStageiv
  , glGetQueryIndexediv
  , glGetSubroutineIndex
  , glGetSubroutineUniformLocation
  , glGetUniformSubroutineuiv
  , glGetUniformdv
  , glIsTransformFeedback
  , glMinSampleShading
  , glPatchParameterfv
  , glPatchParameteri
  , glPauseTransformFeedback
  , glResumeTransformFeedback
  , glUniform1d
  , glUniform1dv
  , glUniform2d
  , glUniform2dv
  , glUniform3d
  , glUniform3dv
  , glUniform4d
  , glUniform4dv
  , glUniformMatrix2dv
  , glUniformMatrix2x3dv
  , glUniformMatrix2x4dv
  , glUniformMatrix3dv
  , glUniformMatrix3x2dv
  , glUniformMatrix3x4dv
  , glUniformMatrix4dv
  , glUniformMatrix4x2dv
  , glUniformMatrix4x3dv
  , glUniformSubroutinesuiv
  , pattern GL_ACTIVE_SUBROUTINES
  , pattern GL_ACTIVE_SUBROUTINE_MAX_LENGTH
  , pattern GL_ACTIVE_SUBROUTINE_UNIFORMS
  , pattern GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS
  , pattern GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH
  , pattern GL_COMPATIBLE_SUBROUTINES
  , pattern GL_DOUBLE_MAT2
  , pattern GL_DOUBLE_MAT2x3
  , pattern GL_DOUBLE_MAT2x4
  , pattern GL_DOUBLE_MAT3
  , pattern GL_DOUBLE_MAT3x2
  , pattern GL_DOUBLE_MAT3x4
  , pattern GL_DOUBLE_MAT4
  , pattern GL_DOUBLE_MAT4x2
  , pattern GL_DOUBLE_MAT4x3
  , pattern GL_DOUBLE_VEC2
  , pattern GL_DOUBLE_VEC3
  , pattern GL_DOUBLE_VEC4
  , pattern GL_DRAW_INDIRECT_BUFFER
  , pattern GL_DRAW_INDIRECT_BUFFER_BINDING
  , pattern GL_FRACTIONAL_EVEN
  , pattern GL_FRACTIONAL_ODD
  , pattern GL_FRAGMENT_INTERPOLATION_OFFSET_BITS
  , pattern GL_GEOMETRY_SHADER_INVOCATIONS
  , pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY
  , pattern GL_ISOLINES
  , pattern GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS
  , pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET
  , pattern GL_MAX_GEOMETRY_SHADER_INVOCATIONS
  , pattern GL_MAX_PATCH_VERTICES
  , pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET
  , pattern GL_MAX_SUBROUTINES
  , pattern GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS
  , pattern GL_MAX_TESS_CONTROL_INPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS
  , pattern GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS
  , pattern GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS
  , pattern GL_MAX_TESS_GEN_LEVEL
  , pattern GL_MAX_TESS_PATCH_COMPONENTS
  , pattern GL_MAX_TRANSFORM_FEEDBACK_BUFFERS
  , pattern GL_MAX_VERTEX_STREAMS
  , pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
  , pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET
  , pattern GL_MIN_SAMPLE_SHADING_VALUE
  , pattern GL_NUM_COMPATIBLE_SUBROUTINES
  , pattern GL_PATCHES
  , pattern GL_PATCH_DEFAULT_INNER_LEVEL
  , pattern GL_PATCH_DEFAULT_OUTER_LEVEL
  , pattern GL_PATCH_VERTICES
  , pattern GL_PROXY_TEXTURE_CUBE_MAP_ARRAY
  , pattern GL_QUADS
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW
  , pattern GL_SAMPLE_SHADING
  , pattern GL_TESS_CONTROL_OUTPUT_VERTICES
  , pattern GL_TESS_CONTROL_SHADER
  , pattern GL_TESS_EVALUATION_SHADER
  , pattern GL_TESS_GEN_MODE
  , pattern GL_TESS_GEN_POINT_MODE
  , pattern GL_TESS_GEN_SPACING
  , pattern GL_TESS_GEN_VERTEX_ORDER
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY
  , pattern GL_TEXTURE_CUBE_MAP_ARRAY
  , pattern GL_TRANSFORM_FEEDBACK
  , pattern GL_TRANSFORM_FEEDBACK_BINDING
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED
  , pattern GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER
  , pattern GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY
) where

import Graphics.GL.Internal.Shared

import Graphics.GL.Core33

pattern GL_PROXY_TEXTURE_CUBE_MAP_ARRAY  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_CUBE_MAP_ARRAY = 0x900B