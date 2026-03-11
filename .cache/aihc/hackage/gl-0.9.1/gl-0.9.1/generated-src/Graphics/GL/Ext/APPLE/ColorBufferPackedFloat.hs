-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.ColorBufferPackedFloat (
  -- * Extension Support
    gl_APPLE_color_buffer_packed_float
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_color_buffer_packed_float.txt GL_APPLE_color_buffer_packed_float> extension is available.

gl_APPLE_color_buffer_packed_float :: Bool
gl_APPLE_color_buffer_packed_float = member "GL_APPLE_color_buffer_packed_float" extensions
{-# NOINLINE gl_APPLE_color_buffer_packed_float #-}