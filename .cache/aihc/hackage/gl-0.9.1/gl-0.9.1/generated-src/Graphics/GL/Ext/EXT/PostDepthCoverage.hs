-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PostDepthCoverage (
  -- * Extension Support
    gl_EXT_post_depth_coverage
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/post_depth_coverage.txt GL_EXT_post_depth_coverage> extension is available.

gl_EXT_post_depth_coverage :: Bool
gl_EXT_post_depth_coverage = member "GL_EXT_post_depth_coverage" extensions
{-# NOINLINE gl_EXT_post_depth_coverage #-}