-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.RobustnessIsolation (
  -- * Extension Support
    gl_ARB_robustness_isolation
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/robustness_isolation.txt GL_ARB_robustness_isolation> extension is available.

gl_ARB_robustness_isolation :: Bool
gl_ARB_robustness_isolation = member "GL_ARB_robustness_isolation" extensions
{-# NOINLINE gl_ARB_robustness_isolation #-}