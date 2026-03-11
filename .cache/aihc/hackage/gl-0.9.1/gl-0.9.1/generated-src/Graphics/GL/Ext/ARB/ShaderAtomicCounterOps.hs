-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderAtomicCounterOps (
  -- * Extension Support
    gl_ARB_shader_atomic_counter_ops
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_atomic_counter_ops.txt GL_ARB_shader_atomic_counter_ops> extension is available.

gl_ARB_shader_atomic_counter_ops :: Bool
gl_ARB_shader_atomic_counter_ops = member "GL_ARB_shader_atomic_counter_ops" extensions
{-# NOINLINE gl_ARB_shader_atomic_counter_ops #-}