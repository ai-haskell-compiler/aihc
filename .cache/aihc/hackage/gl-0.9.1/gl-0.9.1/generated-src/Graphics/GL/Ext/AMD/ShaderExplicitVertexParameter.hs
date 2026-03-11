-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.ShaderExplicitVertexParameter (
  -- * Extension Support
    gl_AMD_shader_explicit_vertex_parameter
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/shader_explicit_vertex_parameter.txt GL_AMD_shader_explicit_vertex_parameter> extension is available.

gl_AMD_shader_explicit_vertex_parameter :: Bool
gl_AMD_shader_explicit_vertex_parameter = member "GL_AMD_shader_explicit_vertex_parameter" extensions
{-# NOINLINE gl_AMD_shader_explicit_vertex_parameter #-}