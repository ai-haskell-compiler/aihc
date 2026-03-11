-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderTextureLod (
  -- * Extension Support
    gl_ARB_shader_texture_lod
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_texture_lod.txt GL_ARB_shader_texture_lod> extension is available.

gl_ARB_shader_texture_lod :: Bool
gl_ARB_shader_texture_lod = member "GL_ARB_shader_texture_lod" extensions
{-# NOINLINE gl_ARB_shader_texture_lod #-}