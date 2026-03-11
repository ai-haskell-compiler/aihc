-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.GeometryShaderPassthrough (
  -- * Extension Support
    gl_NV_geometry_shader_passthrough
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/geometry_shader_passthrough.txt GL_NV_geometry_shader_passthrough> extension is available.

gl_NV_geometry_shader_passthrough :: Bool
gl_NV_geometry_shader_passthrough = member "GL_NV_geometry_shader_passthrough" extensions
{-# NOINLINE gl_NV_geometry_shader_passthrough #-}