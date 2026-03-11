-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.ShaderStencilExport (
  -- * Extension Support
    gl_AMD_shader_stencil_export
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/shader_stencil_export.txt GL_AMD_shader_stencil_export> extension is available.

gl_AMD_shader_stencil_export :: Bool
gl_AMD_shader_stencil_export = member "GL_AMD_shader_stencil_export" extensions
{-# NOINLINE gl_AMD_shader_stencil_export #-}