-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.VertexShaderViewportIndex (
  -- * Extension Support
    gl_AMD_vertex_shader_viewport_index
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/vertex_shader_viewport_index.txt GL_AMD_vertex_shader_viewport_index> extension is available.

gl_AMD_vertex_shader_viewport_index :: Bool
gl_AMD_vertex_shader_viewport_index = member "GL_AMD_vertex_shader_viewport_index" extensions
{-# NOINLINE gl_AMD_vertex_shader_viewport_index #-}