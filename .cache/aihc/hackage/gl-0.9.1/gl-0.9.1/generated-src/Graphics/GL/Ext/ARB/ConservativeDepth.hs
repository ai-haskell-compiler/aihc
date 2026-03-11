-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ConservativeDepth (
  -- * Extension Support
    gl_ARB_conservative_depth
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/conservative_depth.txt GL_ARB_conservative_depth> extension is available.

gl_ARB_conservative_depth :: Bool
gl_ARB_conservative_depth = member "GL_ARB_conservative_depth" extensions
{-# NOINLINE gl_ARB_conservative_depth #-}