-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexType10f11f11fRev (
  -- * Extension Support
    gl_ARB_vertex_type_10f_11f_11f_rev

  -- * GL_ARB_vertex_type_10f_11f_11f_rev
  , pattern GL_UNSIGNED_INT_10F_11F_11F_REV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_type_10f_11f_11f_rev.txt GL_ARB_vertex_type_10f_11f_11f_rev> extension is available.

gl_ARB_vertex_type_10f_11f_11f_rev :: Bool
gl_ARB_vertex_type_10f_11f_11f_rev = member "GL_ARB_vertex_type_10f_11f_11f_rev" extensions
{-# NOINLINE gl_ARB_vertex_type_10f_11f_11f_rev #-}