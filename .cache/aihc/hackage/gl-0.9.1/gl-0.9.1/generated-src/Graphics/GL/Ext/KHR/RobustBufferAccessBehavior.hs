-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.RobustBufferAccessBehavior (
  -- * Extension Support
    gl_KHR_robust_buffer_access_behavior
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/KHR/robust_buffer_access_behavior.txt GL_KHR_robust_buffer_access_behavior> extension is available.

gl_KHR_robust_buffer_access_behavior :: Bool
gl_KHR_robust_buffer_access_behavior = member "GL_KHR_robust_buffer_access_behavior" extensions
{-# NOINLINE gl_KHR_robust_buffer_access_behavior #-}