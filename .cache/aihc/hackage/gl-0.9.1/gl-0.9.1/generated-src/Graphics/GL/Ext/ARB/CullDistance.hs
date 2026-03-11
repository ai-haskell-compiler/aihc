-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.CullDistance (
  -- * Extension Support
    gl_ARB_cull_distance

  -- * GL_ARB_cull_distance
  , pattern GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES
  , pattern GL_MAX_CULL_DISTANCES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/cull_distance.txt GL_ARB_cull_distance> extension is available.

gl_ARB_cull_distance :: Bool
gl_ARB_cull_distance = member "GL_ARB_cull_distance" extensions
{-# NOINLINE gl_ARB_cull_distance #-}