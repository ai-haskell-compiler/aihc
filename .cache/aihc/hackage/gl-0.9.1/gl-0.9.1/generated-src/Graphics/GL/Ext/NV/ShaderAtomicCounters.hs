-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderAtomicCounters (
  -- * Extension Support
    gl_NV_shader_atomic_counters
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_atomic_counters.txt GL_NV_shader_atomic_counters> extension is available.

gl_NV_shader_atomic_counters :: Bool
gl_NV_shader_atomic_counters = member "GL_NV_shader_atomic_counters" extensions
{-# NOINLINE gl_NV_shader_atomic_counters #-}