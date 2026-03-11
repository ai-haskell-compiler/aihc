-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.PostDepthCoverage (
  -- * Extension Support
    gl_ARB_post_depth_coverage
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/post_depth_coverage.txt GL_ARB_post_depth_coverage> extension is available.

gl_ARB_post_depth_coverage :: Bool
gl_ARB_post_depth_coverage = member "GL_ARB_post_depth_coverage" extensions
{-# NOINLINE gl_ARB_post_depth_coverage #-}