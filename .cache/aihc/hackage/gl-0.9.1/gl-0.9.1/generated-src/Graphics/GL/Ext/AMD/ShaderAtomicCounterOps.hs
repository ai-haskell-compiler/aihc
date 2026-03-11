-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.ShaderAtomicCounterOps (
  -- * Extension Support
    gl_AMD_shader_atomic_counter_ops
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/shader_atomic_counter_ops.txt GL_AMD_shader_atomic_counter_ops> extension is available.

gl_AMD_shader_atomic_counter_ops :: Bool
gl_AMD_shader_atomic_counter_ops = member "GL_AMD_shader_atomic_counter_ops" extensions
{-# NOINLINE gl_AMD_shader_atomic_counter_ops #-}