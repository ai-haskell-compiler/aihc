-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexArrayObject (
  -- * Extension Support
    gl_ARB_vertex_array_object

  -- * GL_ARB_vertex_array_object
  , glBindVertexArray
  , glDeleteVertexArrays
  , glGenVertexArrays
  , glIsVertexArray
  , pattern GL_VERTEX_ARRAY_BINDING
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_array_object.txt GL_ARB_vertex_array_object> extension is available.

gl_ARB_vertex_array_object :: Bool
gl_ARB_vertex_array_object = member "GL_ARB_vertex_array_object" extensions
{-# NOINLINE gl_ARB_vertex_array_object #-}