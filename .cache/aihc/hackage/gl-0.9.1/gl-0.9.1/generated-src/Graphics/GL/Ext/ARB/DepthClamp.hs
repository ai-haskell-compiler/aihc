-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DepthClamp (
  -- * Extension Support
    gl_ARB_depth_clamp

  -- * GL_ARB_depth_clamp
  , pattern GL_DEPTH_CLAMP
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/depth_clamp.txt GL_ARB_depth_clamp> extension is available.

gl_ARB_depth_clamp :: Bool
gl_ARB_depth_clamp = member "GL_ARB_depth_clamp" extensions
{-# NOINLINE gl_ARB_depth_clamp #-}