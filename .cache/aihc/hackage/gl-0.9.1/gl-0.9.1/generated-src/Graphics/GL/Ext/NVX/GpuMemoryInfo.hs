-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NVX.GpuMemoryInfo (
  -- * Extension Support
    gl_NVX_gpu_memory_info

  -- * GL_NVX_gpu_memory_info
  , pattern GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX
  , pattern GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX
  , pattern GL_GPU_MEMORY_INFO_EVICTED_MEMORY_NVX
  , pattern GL_GPU_MEMORY_INFO_EVICTION_COUNT_NVX
  , pattern GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NVX/gpu_memory_info.txt GL_NVX_gpu_memory_info> extension is available.

gl_NVX_gpu_memory_info :: Bool
gl_NVX_gpu_memory_info = member "GL_NVX_gpu_memory_info" extensions
{-# NOINLINE gl_NVX_gpu_memory_info #-}

pattern GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX  :: (Eq a, Num a) => a

pattern GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX = 0x9049

pattern GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX  :: (Eq a, Num a) => a

pattern GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX = 0x9047

pattern GL_GPU_MEMORY_INFO_EVICTED_MEMORY_NVX  :: (Eq a, Num a) => a

pattern GL_GPU_MEMORY_INFO_EVICTED_MEMORY_NVX = 0x904B

pattern GL_GPU_MEMORY_INFO_EVICTION_COUNT_NVX  :: (Eq a, Num a) => a

pattern GL_GPU_MEMORY_INFO_EVICTION_COUNT_NVX = 0x904A

pattern GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX  :: (Eq a, Num a) => a

pattern GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX = 0x9048