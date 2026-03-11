-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderThreadShuffle (
  -- * Extension Support
    gl_NV_shader_thread_shuffle
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_thread_shuffle.txt GL_NV_shader_thread_shuffle> extension is available.

gl_NV_shader_thread_shuffle :: Bool
gl_NV_shader_thread_shuffle = member "GL_NV_shader_thread_shuffle" extensions
{-# NOINLINE gl_NV_shader_thread_shuffle #-}