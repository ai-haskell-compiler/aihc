-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.GpuShader5 (
  -- * Extension Support
    gl_EXT_gpu_shader5
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_gpu_shader5.txt GL_EXT_gpu_shader5> extension is available.

gl_EXT_gpu_shader5 :: Bool
gl_EXT_gpu_shader5 = member "GL_EXT_gpu_shader5" extensions
{-# NOINLINE gl_EXT_gpu_shader5 #-}