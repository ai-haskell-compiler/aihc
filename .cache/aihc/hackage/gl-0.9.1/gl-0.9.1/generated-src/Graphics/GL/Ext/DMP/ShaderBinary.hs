-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.DMP.ShaderBinary (
  -- * Extension Support
    gl_DMP_shader_binary

  -- * GL_DMP_shader_binary
  , pattern GL_SHADER_BINARY_DMP
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/DMP/DMP_shader_binary.txt GL_DMP_shader_binary> extension is available.

gl_DMP_shader_binary :: Bool
gl_DMP_shader_binary = member "GL_DMP_shader_binary" extensions
{-# NOINLINE gl_DMP_shader_binary #-}

pattern GL_SHADER_BINARY_DMP  :: (Eq a, Num a) => a

pattern GL_SHADER_BINARY_DMP = 0x9250