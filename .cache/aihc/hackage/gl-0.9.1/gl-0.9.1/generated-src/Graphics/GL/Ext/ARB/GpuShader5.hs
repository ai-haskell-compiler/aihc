-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.GpuShader5 (
  -- * Extension Support
    gl_ARB_gpu_shader5

  -- * GL_ARB_gpu_shader5
  , pattern GL_FRAGMENT_INTERPOLATION_OFFSET_BITS
  , pattern GL_GEOMETRY_SHADER_INVOCATIONS
  , pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET
  , pattern GL_MAX_GEOMETRY_SHADER_INVOCATIONS
  , pattern GL_MAX_VERTEX_STREAMS
  , pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/gpu_shader5.txt GL_ARB_gpu_shader5> extension is available.

gl_ARB_gpu_shader5 :: Bool
gl_ARB_gpu_shader5 = member "GL_ARB_gpu_shader5" extensions
{-# NOINLINE gl_ARB_gpu_shader5 #-}