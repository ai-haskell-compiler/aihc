-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderAtomicFp16Vector (
  -- * Extension Support
    gl_NV_shader_atomic_fp16_vector
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_atomic_fp16_vector.txt GL_NV_shader_atomic_fp16_vector> extension is available.

gl_NV_shader_atomic_fp16_vector :: Bool
gl_NV_shader_atomic_fp16_vector = member "GL_NV_shader_atomic_fp16_vector" extensions
{-# NOINLINE gl_NV_shader_atomic_fp16_vector #-}