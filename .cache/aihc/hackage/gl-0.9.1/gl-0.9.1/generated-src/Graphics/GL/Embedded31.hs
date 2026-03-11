-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Embedded31 (
  -- * Graphics.GL.Embedded31
    module Graphics.GL.Embedded30
  , glActiveShaderProgram
  , glBindImageTexture
  , glBindProgramPipeline
  , glBindVertexBuffer
  , glCreateShaderProgramv
  , glDeleteProgramPipelines
  , glDispatchCompute
  , glDispatchComputeIndirect
  , glDrawArraysIndirect
  , glDrawElementsIndirect
  , glFramebufferParameteri
  , glGenProgramPipelines
  , glGetBooleani_v
  , glGetFramebufferParameteriv
  , glGetMultisamplefv
  , glGetProgramInterfaceiv
  , glGetProgramPipelineInfoLog
  , glGetProgramPipelineiv
  , glGetProgramResourceIndex
  , glGetProgramResourceLocation
  , glGetProgramResourceName
  , glGetProgramResourceiv
  , glGetTexLevelParameterfv
  , glGetTexLevelParameteriv
  , glIsProgramPipeline
  , glMemoryBarrier
  , glMemoryBarrierByRegion
  , glProgramUniform1f
  , glProgramUniform1fv
  , glProgramUniform1i
  , glProgramUniform1iv
  , glProgramUniform1ui
  , glProgramUniform1uiv
  , glProgramUniform2f
  , glProgramUniform2fv
  , glProgramUniform2i
  , glProgramUniform2iv
  , glProgramUniform2ui
  , glProgramUniform2uiv
  , glProgramUniform3f
  , glProgramUniform3fv
  , glProgramUniform3i
  , glProgramUniform3iv
  , glProgramUniform3ui
  , glProgramUniform3uiv
  , glProgramUniform4f
  , glProgramUniform4fv
  , glProgramUniform4i
  , glProgramUniform4iv
  , glProgramUniform4ui
  , glProgramUniform4uiv
  , glProgramUniformMatrix2fv
  , glProgramUniformMatrix2x3fv
  , glProgramUniformMatrix2x4fv
  , glProgramUniformMatrix3fv
  , glProgramUniformMatrix3x2fv
  , glProgramUniformMatrix3x4fv
  , glProgramUniformMatrix4fv
  , glProgramUniformMatrix4x2fv
  , glProgramUniformMatrix4x3fv
  , glSampleMaski
  , glTexStorage2DMultisample
  , glUseProgramStages
  , glValidateProgramPipeline
  , glVertexAttribBinding
  , glVertexAttribFormat
  , glVertexAttribIFormat
  , glVertexBindingDivisor
  , pattern GL_ACTIVE_ATOMIC_COUNTER_BUFFERS
  , pattern GL_ACTIVE_PROGRAM
  , pattern GL_ACTIVE_RESOURCES
  , pattern GL_ACTIVE_VARIABLES
  , pattern GL_ALL_BARRIER_BITS
  , pattern GL_ALL_SHADER_BITS
  , pattern GL_ARRAY_SIZE
  , pattern GL_ARRAY_STRIDE
  , pattern GL_ATOMIC_COUNTER_BARRIER_BIT
  , pattern GL_ATOMIC_COUNTER_BUFFER
  , pattern GL_ATOMIC_COUNTER_BUFFER_BINDING
  , pattern GL_ATOMIC_COUNTER_BUFFER_INDEX
  , pattern GL_ATOMIC_COUNTER_BUFFER_SIZE
  , pattern GL_ATOMIC_COUNTER_BUFFER_START
  , pattern GL_BLOCK_INDEX
  , pattern GL_BUFFER_BINDING
  , pattern GL_BUFFER_DATA_SIZE
  , pattern GL_BUFFER_UPDATE_BARRIER_BIT
  , pattern GL_BUFFER_VARIABLE
  , pattern GL_COMMAND_BARRIER_BIT
  , pattern GL_COMPUTE_SHADER
  , pattern GL_COMPUTE_SHADER_BIT
  , pattern GL_COMPUTE_WORK_GROUP_SIZE
  , pattern GL_DEPTH_STENCIL_TEXTURE_MODE
  , pattern GL_DISPATCH_INDIRECT_BUFFER
  , pattern GL_DISPATCH_INDIRECT_BUFFER_BINDING
  , pattern GL_DRAW_INDIRECT_BUFFER
  , pattern GL_DRAW_INDIRECT_BUFFER_BINDING
  , pattern GL_ELEMENT_ARRAY_BARRIER_BIT
  , pattern GL_FRAGMENT_SHADER_BIT
  , pattern GL_FRAMEBUFFER_BARRIER_BIT
  , pattern GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS
  , pattern GL_FRAMEBUFFER_DEFAULT_HEIGHT
  , pattern GL_FRAMEBUFFER_DEFAULT_SAMPLES
  , pattern GL_FRAMEBUFFER_DEFAULT_WIDTH
  , pattern GL_IMAGE_2D
  , pattern GL_IMAGE_2D_ARRAY
  , pattern GL_IMAGE_3D
  , pattern GL_IMAGE_BINDING_ACCESS
  , pattern GL_IMAGE_BINDING_FORMAT
  , pattern GL_IMAGE_BINDING_LAYER
  , pattern GL_IMAGE_BINDING_LAYERED
  , pattern GL_IMAGE_BINDING_LEVEL
  , pattern GL_IMAGE_BINDING_NAME
  , pattern GL_IMAGE_CUBE
  , pattern GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS
  , pattern GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE
  , pattern GL_IMAGE_FORMAT_COMPATIBILITY_TYPE
  , pattern GL_INT_IMAGE_2D
  , pattern GL_INT_IMAGE_2D_ARRAY
  , pattern GL_INT_IMAGE_3D
  , pattern GL_INT_IMAGE_CUBE
  , pattern GL_INT_SAMPLER_2D_MULTISAMPLE
  , pattern GL_IS_ROW_MAJOR
  , pattern GL_LOCATION
  , pattern GL_MATRIX_STRIDE
  , pattern GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS
  , pattern GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE
  , pattern GL_MAX_COLOR_TEXTURE_SAMPLES
  , pattern GL_MAX_COMBINED_ATOMIC_COUNTERS
  , pattern GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMBINED_IMAGE_UNIFORMS
  , pattern GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES
  , pattern GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_COMPUTE_ATOMIC_COUNTERS
  , pattern GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_COMPUTE_IMAGE_UNIFORMS
  , pattern GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_COMPUTE_SHARED_MEMORY_SIZE
  , pattern GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_COMPUTE_UNIFORM_BLOCKS
  , pattern GL_MAX_COMPUTE_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMPUTE_WORK_GROUP_COUNT
  , pattern GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS
  , pattern GL_MAX_COMPUTE_WORK_GROUP_SIZE
  , pattern GL_MAX_DEPTH_TEXTURE_SAMPLES
  , pattern GL_MAX_FRAGMENT_ATOMIC_COUNTERS
  , pattern GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_FRAGMENT_IMAGE_UNIFORMS
  , pattern GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_FRAMEBUFFER_HEIGHT
  , pattern GL_MAX_FRAMEBUFFER_SAMPLES
  , pattern GL_MAX_FRAMEBUFFER_WIDTH
  , pattern GL_MAX_IMAGE_UNITS
  , pattern GL_MAX_INTEGER_SAMPLES
  , pattern GL_MAX_NAME_LENGTH
  , pattern GL_MAX_NUM_ACTIVE_VARIABLES
  , pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET
  , pattern GL_MAX_SAMPLE_MASK_WORDS
  , pattern GL_MAX_SHADER_STORAGE_BLOCK_SIZE
  , pattern GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS
  , pattern GL_MAX_UNIFORM_LOCATIONS
  , pattern GL_MAX_VERTEX_ATOMIC_COUNTERS
  , pattern GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_VERTEX_ATTRIB_BINDINGS
  , pattern GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET
  , pattern GL_MAX_VERTEX_ATTRIB_STRIDE
  , pattern GL_MAX_VERTEX_IMAGE_UNIFORMS
  , pattern GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS
  , pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET
  , pattern GL_NAME_LENGTH
  , pattern GL_NUM_ACTIVE_VARIABLES
  , pattern GL_OFFSET
  , pattern GL_PIXEL_BUFFER_BARRIER_BIT
  , pattern GL_PROGRAM_INPUT
  , pattern GL_PROGRAM_OUTPUT
  , pattern GL_PROGRAM_PIPELINE_BINDING
  , pattern GL_PROGRAM_SEPARABLE
  , pattern GL_READ_ONLY
  , pattern GL_READ_WRITE
  , pattern GL_REFERENCED_BY_COMPUTE_SHADER
  , pattern GL_REFERENCED_BY_FRAGMENT_SHADER
  , pattern GL_REFERENCED_BY_VERTEX_SHADER
  , pattern GL_SAMPLER_2D_MULTISAMPLE
  , pattern GL_SAMPLE_MASK
  , pattern GL_SAMPLE_MASK_VALUE
  , pattern GL_SAMPLE_POSITION
  , pattern GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
  , pattern GL_SHADER_STORAGE_BARRIER_BIT
  , pattern GL_SHADER_STORAGE_BLOCK
  , pattern GL_SHADER_STORAGE_BUFFER
  , pattern GL_SHADER_STORAGE_BUFFER_BINDING
  , pattern GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT
  , pattern GL_SHADER_STORAGE_BUFFER_SIZE
  , pattern GL_SHADER_STORAGE_BUFFER_START
  , pattern GL_STENCIL_INDEX
  , pattern GL_TEXTURE_2D_MULTISAMPLE
  , pattern GL_TEXTURE_ALPHA_SIZE
  , pattern GL_TEXTURE_ALPHA_TYPE
  , pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE
  , pattern GL_TEXTURE_BLUE_SIZE
  , pattern GL_TEXTURE_BLUE_TYPE
  , pattern GL_TEXTURE_COMPRESSED
  , pattern GL_TEXTURE_DEPTH
  , pattern GL_TEXTURE_DEPTH_SIZE
  , pattern GL_TEXTURE_DEPTH_TYPE
  , pattern GL_TEXTURE_FETCH_BARRIER_BIT
  , pattern GL_TEXTURE_FIXED_SAMPLE_LOCATIONS
  , pattern GL_TEXTURE_GREEN_SIZE
  , pattern GL_TEXTURE_GREEN_TYPE
  , pattern GL_TEXTURE_HEIGHT
  , pattern GL_TEXTURE_INTERNAL_FORMAT
  , pattern GL_TEXTURE_RED_SIZE
  , pattern GL_TEXTURE_RED_TYPE
  , pattern GL_TEXTURE_SAMPLES
  , pattern GL_TEXTURE_SHARED_SIZE
  , pattern GL_TEXTURE_STENCIL_SIZE
  , pattern GL_TEXTURE_UPDATE_BARRIER_BIT
  , pattern GL_TEXTURE_WIDTH
  , pattern GL_TOP_LEVEL_ARRAY_SIZE
  , pattern GL_TOP_LEVEL_ARRAY_STRIDE
  , pattern GL_TRANSFORM_FEEDBACK_BARRIER_BIT
  , pattern GL_TRANSFORM_FEEDBACK_VARYING
  , pattern GL_TYPE
  , pattern GL_UNIFORM
  , pattern GL_UNIFORM_BARRIER_BIT
  , pattern GL_UNIFORM_BLOCK
  , pattern GL_UNSIGNED_INT_ATOMIC_COUNTER
  , pattern GL_UNSIGNED_INT_IMAGE_2D
  , pattern GL_UNSIGNED_INT_IMAGE_2D_ARRAY
  , pattern GL_UNSIGNED_INT_IMAGE_3D
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
  , pattern GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT
  , pattern GL_VERTEX_ATTRIB_BINDING
  , pattern GL_VERTEX_ATTRIB_RELATIVE_OFFSET
  , pattern GL_VERTEX_BINDING_BUFFER
  , pattern GL_VERTEX_BINDING_DIVISOR
  , pattern GL_VERTEX_BINDING_OFFSET
  , pattern GL_VERTEX_BINDING_STRIDE
  , pattern GL_VERTEX_SHADER_BIT
  , pattern GL_WRITE_ONLY
) where

import Graphics.GL.Internal.Shared

import Graphics.GL.Embedded30