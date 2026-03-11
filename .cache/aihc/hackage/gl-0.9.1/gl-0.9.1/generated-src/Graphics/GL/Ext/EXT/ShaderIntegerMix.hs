-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderIntegerMix (
  -- * Extension Support
    gl_EXT_shader_integer_mix
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_shader_integer_mix.txt GL_EXT_shader_integer_mix> extension is available.

gl_EXT_shader_integer_mix :: Bool
gl_EXT_shader_integer_mix = member "GL_EXT_shader_integer_mix" extensions
{-# NOINLINE gl_EXT_shader_integer_mix #-}