-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexProgram3 (
  -- * Extension Support
    gl_NV_vertex_program3

  -- * GL_NV_vertex_program3
  , pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_program3.txt GL_NV_vertex_program3> extension is available.

gl_NV_vertex_program3 :: Bool
gl_NV_vertex_program3 = member "GL_NV_vertex_program3" extensions
{-# NOINLINE gl_NV_vertex_program3 #-}