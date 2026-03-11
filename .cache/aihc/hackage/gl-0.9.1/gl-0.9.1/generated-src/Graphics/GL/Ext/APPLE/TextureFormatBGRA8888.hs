-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.TextureFormatBGRA8888 (
  -- * Extension Support
    gl_APPLE_texture_format_BGRA8888

  -- * GL_APPLE_texture_format_BGRA8888
  , pattern GL_BGRA8_EXT
  , pattern GL_BGRA_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_texture_format_BGRA8888.txt GL_APPLE_texture_format_BGRA8888> extension is available.

gl_APPLE_texture_format_BGRA8888 :: Bool
gl_APPLE_texture_format_BGRA8888 = member "GL_APPLE_texture_format_BGRA8888" extensions
{-# NOINLINE gl_APPLE_texture_format_BGRA8888 #-}