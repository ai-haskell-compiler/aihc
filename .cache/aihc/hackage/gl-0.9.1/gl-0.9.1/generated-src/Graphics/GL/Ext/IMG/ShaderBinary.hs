-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.ShaderBinary (
  -- * Extension Support
    gl_IMG_shader_binary

  -- * GL_IMG_shader_binary
  , pattern GL_SGX_BINARY_IMG
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_shader_binary.txt GL_IMG_shader_binary> extension is available.

gl_IMG_shader_binary :: Bool
gl_IMG_shader_binary = member "GL_IMG_shader_binary" extensions
{-# NOINLINE gl_IMG_shader_binary #-}

pattern GL_SGX_BINARY_IMG  :: (Eq a, Num a) => a

pattern GL_SGX_BINARY_IMG = 0x8C0A