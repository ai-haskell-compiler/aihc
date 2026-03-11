-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.RobustBufferAccessBehavior (
  -- * Extension Support
    gl_ARB_robust_buffer_access_behavior
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/robust_buffer_access_behavior.txt GL_ARB_robust_buffer_access_behavior> extension is available.

gl_ARB_robust_buffer_access_behavior :: Bool
gl_ARB_robust_buffer_access_behavior = member "GL_ARB_robust_buffer_access_behavior" extensions
{-# NOINLINE gl_ARB_robust_buffer_access_behavior #-}