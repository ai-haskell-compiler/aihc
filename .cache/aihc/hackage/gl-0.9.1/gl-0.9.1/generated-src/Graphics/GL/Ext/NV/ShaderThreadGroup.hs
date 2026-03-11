-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderThreadGroup (
  -- * Extension Support
    gl_NV_shader_thread_group

  -- * GL_NV_shader_thread_group
  , pattern GL_SM_COUNT_NV
  , pattern GL_WARPS_PER_SM_NV
  , pattern GL_WARP_SIZE_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_thread_group.txt GL_NV_shader_thread_group> extension is available.

gl_NV_shader_thread_group :: Bool
gl_NV_shader_thread_group = member "GL_NV_shader_thread_group" extensions
{-# NOINLINE gl_NV_shader_thread_group #-}

pattern GL_SM_COUNT_NV  :: (Eq a, Num a) => a

pattern GL_SM_COUNT_NV = 0x933B

pattern GL_WARPS_PER_SM_NV  :: (Eq a, Num a) => a

pattern GL_WARPS_PER_SM_NV = 0x933A

pattern GL_WARP_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_WARP_SIZE_NV = 0x9339