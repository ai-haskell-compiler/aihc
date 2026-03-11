-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureMirrorClampToEdge (
  -- * Extension Support
    gl_ARB_texture_mirror_clamp_to_edge

  -- * GL_ARB_texture_mirror_clamp_to_edge
  , pattern GL_MIRROR_CLAMP_TO_EDGE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_mirror_clamp_to_edge.txt GL_ARB_texture_mirror_clamp_to_edge> extension is available.

gl_ARB_texture_mirror_clamp_to_edge :: Bool
gl_ARB_texture_mirror_clamp_to_edge = member "GL_ARB_texture_mirror_clamp_to_edge" extensions
{-# NOINLINE gl_ARB_texture_mirror_clamp_to_edge #-}