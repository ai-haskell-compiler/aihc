-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderAtomicFloat64 (
  -- * Extension Support
    gl_NV_shader_atomic_float64
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_atomic_float64.txt GL_NV_shader_atomic_float64> extension is available.

gl_NV_shader_atomic_float64 :: Bool
gl_NV_shader_atomic_float64 = member "GL_NV_shader_atomic_float64" extensions
{-# NOINLINE gl_NV_shader_atomic_float64 #-}