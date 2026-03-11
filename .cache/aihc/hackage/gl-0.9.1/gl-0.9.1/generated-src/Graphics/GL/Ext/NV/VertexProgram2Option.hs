-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexProgram2Option (
  -- * Extension Support
    gl_NV_vertex_program2_option

  -- * GL_NV_vertex_program2_option
  , pattern GL_MAX_PROGRAM_CALL_DEPTH_NV
  , pattern GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_program2_option.txt GL_NV_vertex_program2_option> extension is available.

gl_NV_vertex_program2_option :: Bool
gl_NV_vertex_program2_option = member "GL_NV_vertex_program2_option" extensions
{-# NOINLINE gl_NV_vertex_program2_option #-}