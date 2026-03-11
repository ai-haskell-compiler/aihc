-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ComputeShader (
  -- * Extension Support
    gl_ARB_compute_shader

  -- * GL_ARB_compute_shader
  , glDispatchCompute
  , glDispatchComputeIndirect
  , pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_COMPUTE_SHADER
  , pattern GL_COMPUTE_SHADER
  , pattern GL_COMPUTE_SHADER_BIT
  , pattern GL_COMPUTE_WORK_GROUP_SIZE
  , pattern GL_DISPATCH_INDIRECT_BUFFER
  , pattern GL_DISPATCH_INDIRECT_BUFFER_BINDING
  , pattern GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMPUTE_ATOMIC_COUNTERS
  , pattern GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_COMPUTE_IMAGE_UNIFORMS
  , pattern GL_MAX_COMPUTE_SHARED_MEMORY_SIZE
  , pattern GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_COMPUTE_UNIFORM_BLOCKS
  , pattern GL_MAX_COMPUTE_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMPUTE_WORK_GROUP_COUNT
  , pattern GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS
  , pattern GL_MAX_COMPUTE_WORK_GROUP_SIZE
  , pattern GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/compute_shader.txt GL_ARB_compute_shader> extension is available.

gl_ARB_compute_shader :: Bool
gl_ARB_compute_shader = member "GL_ARB_compute_shader" extensions
{-# NOINLINE gl_ARB_compute_shader #-}