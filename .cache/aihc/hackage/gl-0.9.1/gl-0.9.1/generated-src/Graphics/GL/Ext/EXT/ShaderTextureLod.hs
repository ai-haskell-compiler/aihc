-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderTextureLod (
  -- * Extension Support
    gl_EXT_shader_texture_lod
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_shader_texture_lod.txt GL_EXT_shader_texture_lod> extension is available.

gl_EXT_shader_texture_lod :: Bool
gl_EXT_shader_texture_lod = member "GL_EXT_shader_texture_lod" extensions
{-# NOINLINE gl_EXT_shader_texture_lod #-}