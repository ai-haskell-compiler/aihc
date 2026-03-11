-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderNonConstantGlobalInitializers (
  -- * Extension Support
    gl_EXT_shader_non_constant_global_initializers
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_shader_non_constant_global_initializers.txt GL_EXT_shader_non_constant_global_initializers> extension is available.

gl_EXT_shader_non_constant_global_initializers :: Bool
gl_EXT_shader_non_constant_global_initializers = member "GL_EXT_shader_non_constant_global_initializers" extensions
{-# NOINLINE gl_EXT_shader_non_constant_global_initializers #-}