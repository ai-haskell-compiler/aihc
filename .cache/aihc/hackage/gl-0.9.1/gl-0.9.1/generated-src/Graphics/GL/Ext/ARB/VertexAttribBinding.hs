-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexAttribBinding (
  -- * Extension Support
    gl_ARB_vertex_attrib_binding

  -- * GL_ARB_vertex_attrib_binding
  , glBindVertexBuffer
  , glVertexAttribBinding
  , glVertexAttribFormat
  , glVertexAttribIFormat
  , glVertexAttribLFormat
  , glVertexBindingDivisor
  , pattern GL_MAX_VERTEX_ATTRIB_BINDINGS
  , pattern GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET
  , pattern GL_VERTEX_ATTRIB_BINDING
  , pattern GL_VERTEX_ATTRIB_RELATIVE_OFFSET
  , pattern GL_VERTEX_BINDING_DIVISOR
  , pattern GL_VERTEX_BINDING_OFFSET
  , pattern GL_VERTEX_BINDING_STRIDE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_attrib_binding.txt GL_ARB_vertex_attrib_binding> extension is available.

gl_ARB_vertex_attrib_binding :: Bool
gl_ARB_vertex_attrib_binding = member "GL_ARB_vertex_attrib_binding" extensions
{-# NOINLINE gl_ARB_vertex_attrib_binding #-}