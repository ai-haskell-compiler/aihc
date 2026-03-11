-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexType2101010Rev (
  -- * Extension Support
    gl_ARB_vertex_type_2_10_10_10_rev

  -- * GL_ARB_vertex_type_2_10_10_10_rev
  , glColorP3ui
  , glColorP3uiv
  , glColorP4ui
  , glColorP4uiv
  , glMultiTexCoordP1ui
  , glMultiTexCoordP1uiv
  , glMultiTexCoordP2ui
  , glMultiTexCoordP2uiv
  , glMultiTexCoordP3ui
  , glMultiTexCoordP3uiv
  , glMultiTexCoordP4ui
  , glMultiTexCoordP4uiv
  , glNormalP3ui
  , glNormalP3uiv
  , glSecondaryColorP3ui
  , glSecondaryColorP3uiv
  , glTexCoordP1ui
  , glTexCoordP1uiv
  , glTexCoordP2ui
  , glTexCoordP2uiv
  , glTexCoordP3ui
  , glTexCoordP3uiv
  , glTexCoordP4ui
  , glTexCoordP4uiv
  , glVertexAttribP1ui
  , glVertexAttribP1uiv
  , glVertexAttribP2ui
  , glVertexAttribP2uiv
  , glVertexAttribP3ui
  , glVertexAttribP3uiv
  , glVertexAttribP4ui
  , glVertexAttribP4uiv
  , glVertexP2ui
  , glVertexP2uiv
  , glVertexP3ui
  , glVertexP3uiv
  , glVertexP4ui
  , glVertexP4uiv
  , pattern GL_INT_2_10_10_10_REV
  , pattern GL_UNSIGNED_INT_2_10_10_10_REV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_type_2_10_10_10_rev.txt GL_ARB_vertex_type_2_10_10_10_rev> extension is available.

gl_ARB_vertex_type_2_10_10_10_rev :: Bool
gl_ARB_vertex_type_2_10_10_10_rev = member "GL_ARB_vertex_type_2_10_10_10_rev" extensions
{-# NOINLINE gl_ARB_vertex_type_2_10_10_10_rev #-}