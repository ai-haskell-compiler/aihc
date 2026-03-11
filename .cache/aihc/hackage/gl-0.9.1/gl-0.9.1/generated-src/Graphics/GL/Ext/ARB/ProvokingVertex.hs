-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ProvokingVertex (
  -- * Extension Support
    gl_ARB_provoking_vertex

  -- * GL_ARB_provoking_vertex
  , glProvokingVertex
  , pattern GL_FIRST_VERTEX_CONVENTION
  , pattern GL_LAST_VERTEX_CONVENTION
  , pattern GL_PROVOKING_VERTEX
  , pattern GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/provoking_vertex.txt GL_ARB_provoking_vertex> extension is available.

gl_ARB_provoking_vertex :: Bool
gl_ARB_provoking_vertex = member "GL_ARB_provoking_vertex" extensions
{-# NOINLINE gl_ARB_provoking_vertex #-}