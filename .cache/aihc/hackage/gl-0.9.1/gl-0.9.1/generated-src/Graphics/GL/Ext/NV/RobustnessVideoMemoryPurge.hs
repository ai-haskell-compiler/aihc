-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.RobustnessVideoMemoryPurge (
  -- * Extension Support
    gl_NV_robustness_video_memory_purge

  -- * GL_NV_robustness_video_memory_purge
  , pattern GL_PURGED_CONTEXT_RESET_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/robustness_video_memory_purge.txt GL_NV_robustness_video_memory_purge> extension is available.

gl_NV_robustness_video_memory_purge :: Bool
gl_NV_robustness_video_memory_purge = member "GL_NV_robustness_video_memory_purge" extensions
{-# NOINLINE gl_NV_robustness_video_memory_purge #-}

pattern GL_PURGED_CONTEXT_RESET_NV  :: (Eq a, Num a) => a

pattern GL_PURGED_CONTEXT_RESET_NV = 0x92BB