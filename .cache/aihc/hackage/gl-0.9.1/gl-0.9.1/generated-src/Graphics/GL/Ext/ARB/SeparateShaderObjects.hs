-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SeparateShaderObjects (
  -- * Extension Support
    gl_ARB_separate_shader_objects

  -- * GL_ARB_separate_shader_objects
  , glActiveShaderProgram
  , glBindProgramPipeline
  , glCreateShaderProgramv
  , glDeleteProgramPipelines
  , glGenProgramPipelines
  , glGetProgramPipelineInfoLog
  , glGetProgramPipelineiv
  , glIsProgramPipeline
  , glProgramUniform1d
  , glProgramUniform1dv
  , glProgramUniform1f
  , glProgramUniform1fv
  , glProgramUniform1i
  , glProgramUniform1iv
  , glProgramUniform1ui
  , glProgramUniform1uiv
  , glProgramUniform2d
  , glProgramUniform2dv
  , glProgramUniform2f
  , glProgramUniform2fv
  , glProgramUniform2i
  , glProgramUniform2iv
  , glProgramUniform2ui
  , glProgramUniform2uiv
  , glProgramUniform3d
  , glProgramUniform3dv
  , glProgramUniform3f
  , glProgramUniform3fv
  , glProgramUniform3i
  , glProgramUniform3iv
  , glProgramUniform3ui
  , glProgramUniform3uiv
  , glProgramUniform4d
  , glProgramUniform4dv
  , glProgramUniform4f
  , glProgramUniform4fv
  , glProgramUniform4i
  , glProgramUniform4iv
  , glProgramUniform4ui
  , glProgramUniform4uiv
  , glProgramUniformMatrix2dv
  , glProgramUniformMatrix2fv
  , glProgramUniformMatrix2x3dv
  , glProgramUniformMatrix2x3fv
  , glProgramUniformMatrix2x4dv
  , glProgramUniformMatrix2x4fv
  , glProgramUniformMatrix3dv
  , glProgramUniformMatrix3fv
  , glProgramUniformMatrix3x2dv
  , glProgramUniformMatrix3x2fv
  , glProgramUniformMatrix3x4dv
  , glProgramUniformMatrix3x4fv
  , glProgramUniformMatrix4dv
  , glProgramUniformMatrix4fv
  , glProgramUniformMatrix4x2dv
  , glProgramUniformMatrix4x2fv
  , glProgramUniformMatrix4x3dv
  , glProgramUniformMatrix4x3fv
  , glUseProgramStages
  , glValidateProgramPipeline
  , pattern GL_ACTIVE_PROGRAM
  , pattern GL_ALL_SHADER_BITS
  , pattern GL_FRAGMENT_SHADER_BIT
  , pattern GL_GEOMETRY_SHADER_BIT
  , pattern GL_PROGRAM_PIPELINE_BINDING
  , pattern GL_PROGRAM_SEPARABLE
  , pattern GL_TESS_CONTROL_SHADER_BIT
  , pattern GL_TESS_EVALUATION_SHADER_BIT
  , pattern GL_VERTEX_SHADER_BIT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/separate_shader_objects.txt GL_ARB_separate_shader_objects> extension is available.

gl_ARB_separate_shader_objects :: Bool
gl_ARB_separate_shader_objects = member "GL_ARB_separate_shader_objects" extensions
{-# NOINLINE gl_ARB_separate_shader_objects #-}