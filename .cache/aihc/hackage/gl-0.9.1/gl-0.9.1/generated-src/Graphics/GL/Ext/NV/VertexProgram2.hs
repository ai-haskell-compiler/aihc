-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexProgram2 (
  -- * Extension Support
    gl_NV_vertex_program2
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_program2.txt GL_NV_vertex_program2> extension is available.

gl_NV_vertex_program2 :: Bool
gl_NV_vertex_program2 = member "GL_NV_vertex_program2" extensions
{-# NOINLINE gl_NV_vertex_program2 #-}