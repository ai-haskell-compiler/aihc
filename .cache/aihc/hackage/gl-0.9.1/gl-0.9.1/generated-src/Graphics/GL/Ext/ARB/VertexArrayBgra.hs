-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexArrayBgra (
  -- * Extension Support
    gl_ARB_vertex_array_bgra

  -- * GL_ARB_vertex_array_bgra
  , pattern GL_BGRA
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_array_bgra.txt GL_ARB_vertex_array_bgra> extension is available.

gl_ARB_vertex_array_bgra :: Bool
gl_ARB_vertex_array_bgra = member "GL_ARB_vertex_array_bgra" extensions
{-# NOINLINE gl_ARB_vertex_array_bgra #-}