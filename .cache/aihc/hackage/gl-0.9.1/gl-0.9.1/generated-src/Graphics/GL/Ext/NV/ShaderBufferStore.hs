-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderBufferStore (
  -- * Extension Support
    gl_NV_shader_buffer_store

  -- * GL_NV_shader_buffer_store
  , pattern GL_READ_WRITE
  , pattern GL_SHADER_GLOBAL_ACCESS_BARRIER_BIT_NV
  , pattern GL_WRITE_ONLY
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_buffer_store.txt GL_NV_shader_buffer_store> extension is available.

gl_NV_shader_buffer_store :: Bool
gl_NV_shader_buffer_store = member "GL_NV_shader_buffer_store" extensions
{-# NOINLINE gl_NV_shader_buffer_store #-}

pattern GL_SHADER_GLOBAL_ACCESS_BARRIER_BIT_NV  :: (Eq a, Num a) => a

pattern GL_SHADER_GLOBAL_ACCESS_BARRIER_BIT_NV = 0x00000010