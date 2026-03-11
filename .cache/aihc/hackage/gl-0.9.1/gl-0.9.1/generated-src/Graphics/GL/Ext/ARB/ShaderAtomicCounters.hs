-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderAtomicCounters (
  -- * Extension Support
    gl_ARB_shader_atomic_counters

  -- * GL_ARB_shader_atomic_counters
  , glGetActiveAtomicCounterBufferiv
  , pattern GL_ACTIVE_ATOMIC_COUNTER_BUFFERS
  , pattern GL_ATOMIC_COUNTER_BUFFER
  , pattern GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTERS
  , pattern GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTER_INDICES
  , pattern GL_ATOMIC_COUNTER_BUFFER_BINDING
  , pattern GL_ATOMIC_COUNTER_BUFFER_DATA_SIZE
  , pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_FRAGMENT_SHADER
  , pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_GEOMETRY_SHADER
  , pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_CONTROL_SHADER
  , pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_EVALUATION_SHADER
  , pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_VERTEX_SHADER
  , pattern GL_ATOMIC_COUNTER_BUFFER_SIZE
  , pattern GL_ATOMIC_COUNTER_BUFFER_START
  , pattern GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS
  , pattern GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE
  , pattern GL_MAX_COMBINED_ATOMIC_COUNTERS
  , pattern GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_FRAGMENT_ATOMIC_COUNTERS
  , pattern GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_GEOMETRY_ATOMIC_COUNTERS
  , pattern GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS
  , pattern GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS
  , pattern GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS
  , pattern GL_MAX_VERTEX_ATOMIC_COUNTERS
  , pattern GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS
  , pattern GL_UNIFORM_ATOMIC_COUNTER_BUFFER_INDEX
  , pattern GL_UNSIGNED_INT_ATOMIC_COUNTER
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_atomic_counters.txt GL_ARB_shader_atomic_counters> extension is available.

gl_ARB_shader_atomic_counters :: Bool
gl_ARB_shader_atomic_counters = member "GL_ARB_shader_atomic_counters" extensions
{-# NOINLINE gl_ARB_shader_atomic_counters #-}