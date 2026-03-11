-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureFloatLinear (
  -- * Extension Support
    gl_OES_texture_float_linear
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_float_linear.txt GL_OES_texture_float_linear> extension is available.

gl_OES_texture_float_linear :: Bool
gl_OES_texture_float_linear = member "GL_OES_texture_float_linear" extensions
{-# NOINLINE gl_OES_texture_float_linear #-}