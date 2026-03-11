-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexAttrib64bit (
  -- * Extension Support
    gl_ARB_vertex_attrib_64bit

  -- * GL_ARB_vertex_attrib_64bit
  , glGetVertexAttribLdv
  , glVertexAttribL1d
  , glVertexAttribL1dv
  , glVertexAttribL2d
  , glVertexAttribL2dv
  , glVertexAttribL3d
  , glVertexAttribL3dv
  , glVertexAttribL4d
  , glVertexAttribL4dv
  , glVertexAttribLPointer
  , pattern GL_DOUBLE_MAT2
  , pattern GL_DOUBLE_MAT2x3
  , pattern GL_DOUBLE_MAT2x4
  , pattern GL_DOUBLE_MAT3
  , pattern GL_DOUBLE_MAT3x2
  , pattern GL_DOUBLE_MAT3x4
  , pattern GL_DOUBLE_MAT4
  , pattern GL_DOUBLE_MAT4x2
  , pattern GL_DOUBLE_MAT4x3
  , pattern GL_DOUBLE_VEC2
  , pattern GL_DOUBLE_VEC3
  , pattern GL_DOUBLE_VEC4
  , pattern GL_RGB32I
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_attrib_64bit.txt GL_ARB_vertex_attrib_64bit> extension is available.

gl_ARB_vertex_attrib_64bit :: Bool
gl_ARB_vertex_attrib_64bit = member "GL_ARB_vertex_attrib_64bit" extensions
{-# NOINLINE gl_ARB_vertex_attrib_64bit #-}