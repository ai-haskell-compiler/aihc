-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ColorBufferFloat (
  -- * Extension Support
    gl_EXT_color_buffer_float
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_color_buffer_float.txt GL_EXT_color_buffer_float> extension is available.

gl_EXT_color_buffer_float :: Bool
gl_EXT_color_buffer_float = member "GL_EXT_color_buffer_float" extensions
{-# NOINLINE gl_EXT_color_buffer_float #-}