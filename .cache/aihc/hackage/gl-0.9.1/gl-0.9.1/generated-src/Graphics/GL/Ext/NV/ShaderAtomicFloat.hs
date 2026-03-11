-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderAtomicFloat (
  -- * Extension Support
    gl_NV_shader_atomic_float
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_atomic_float.txt GL_NV_shader_atomic_float> extension is available.

gl_NV_shader_atomic_float :: Bool
gl_NV_shader_atomic_float = member "GL_NV_shader_atomic_float" extensions
{-# NOINLINE gl_NV_shader_atomic_float #-}