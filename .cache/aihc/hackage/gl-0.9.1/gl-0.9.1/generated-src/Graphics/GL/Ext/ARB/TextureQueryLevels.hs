-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureQueryLevels (
  -- * Extension Support
    gl_ARB_texture_query_levels
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_query_levels.txt GL_ARB_texture_query_levels> extension is available.

gl_ARB_texture_query_levels :: Bool
gl_ARB_texture_query_levels = member "GL_ARB_texture_query_levels" extensions
{-# NOINLINE gl_ARB_texture_query_levels #-}