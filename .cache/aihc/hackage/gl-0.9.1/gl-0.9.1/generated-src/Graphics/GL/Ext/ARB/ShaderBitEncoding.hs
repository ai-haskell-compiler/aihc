-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderBitEncoding (
  -- * Extension Support
    gl_ARB_shader_bit_encoding
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_bit_encoding.txt GL_ARB_shader_bit_encoding> extension is available.

gl_ARB_shader_bit_encoding :: Bool
gl_ARB_shader_bit_encoding = member "GL_ARB_shader_bit_encoding" extensions
{-# NOINLINE gl_ARB_shader_bit_encoding #-}