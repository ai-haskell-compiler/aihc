-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderImplicitConversions (
  -- * Extension Support
    gl_EXT_shader_implicit_conversions
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_shader_implicit_conversions.txt GL_EXT_shader_implicit_conversions> extension is available.

gl_EXT_shader_implicit_conversions :: Bool
gl_EXT_shader_implicit_conversions = member "GL_EXT_shader_implicit_conversions" extensions
{-# NOINLINE gl_EXT_shader_implicit_conversions #-}