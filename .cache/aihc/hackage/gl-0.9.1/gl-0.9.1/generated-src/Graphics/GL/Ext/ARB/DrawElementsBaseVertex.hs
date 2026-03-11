-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DrawElementsBaseVertex (
  -- * Extension Support
    gl_ARB_draw_elements_base_vertex

  -- * GL_ARB_draw_elements_base_vertex
  , glDrawElementsBaseVertex
  , glDrawElementsInstancedBaseVertex
  , glDrawRangeElementsBaseVertex
  , glMultiDrawElementsBaseVertex
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/draw_elements_base_vertex.txt GL_ARB_draw_elements_base_vertex> extension is available.

gl_ARB_draw_elements_base_vertex :: Bool
gl_ARB_draw_elements_base_vertex = member "GL_ARB_draw_elements_base_vertex" extensions
{-# NOINLINE gl_ARB_draw_elements_base_vertex #-}