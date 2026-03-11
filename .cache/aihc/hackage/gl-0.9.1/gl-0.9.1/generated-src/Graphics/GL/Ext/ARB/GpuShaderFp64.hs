-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.GpuShaderFp64 (
  -- * Extension Support
    gl_ARB_gpu_shader_fp64

  -- * GL_ARB_gpu_shader_fp64
  , glGetUniformdv
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
  , pattern GL_DOUBLE
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
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/gpu_shader_fp64.txt GL_ARB_gpu_shader_fp64> extension is available.

gl_ARB_gpu_shader_fp64 :: Bool
gl_ARB_gpu_shader_fp64 = member "GL_ARB_gpu_shader_fp64" extensions
{-# NOINLINE gl_ARB_gpu_shader_fp64 #-}