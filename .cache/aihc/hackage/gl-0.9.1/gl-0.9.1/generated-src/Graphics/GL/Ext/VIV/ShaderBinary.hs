-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.VIV.ShaderBinary (
  -- * Extension Support
    gl_VIV_shader_binary

  -- * GL_VIV_shader_binary
  , pattern GL_SHADER_BINARY_VIV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/VIV/VIV_shader_binary.txt GL_VIV_shader_binary> extension is available.

gl_VIV_shader_binary :: Bool
gl_VIV_shader_binary = member "GL_VIV_shader_binary" extensions
{-# NOINLINE gl_VIV_shader_binary #-}

pattern GL_SHADER_BINARY_VIV  :: (Eq a, Num a) => a

pattern GL_SHADER_BINARY_VIV = 0x8FC4