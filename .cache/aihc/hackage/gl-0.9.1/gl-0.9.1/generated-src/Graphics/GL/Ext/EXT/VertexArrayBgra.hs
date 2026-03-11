-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.VertexArrayBgra (
  -- * Extension Support
    gl_EXT_vertex_array_bgra

  -- * GL_EXT_vertex_array_bgra
  , pattern GL_BGRA
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/vertex_array_bgra.txt GL_EXT_vertex_array_bgra> extension is available.

gl_EXT_vertex_array_bgra :: Bool
gl_EXT_vertex_array_bgra = member "GL_EXT_vertex_array_bgra" extensions
{-# NOINLINE gl_EXT_vertex_array_bgra #-}