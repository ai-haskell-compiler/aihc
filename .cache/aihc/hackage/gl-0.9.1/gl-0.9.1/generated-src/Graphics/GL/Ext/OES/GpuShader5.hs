-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.GpuShader5 (
  -- * Extension Support
    gl_OES_gpu_shader5
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_gpu_shader5.txt GL_OES_gpu_shader5> extension is available.

gl_OES_gpu_shader5 :: Bool
gl_OES_gpu_shader5 = member "GL_OES_gpu_shader5" extensions
{-# NOINLINE gl_OES_gpu_shader5 #-}