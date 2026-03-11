-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Rgb8Rgba8 (
  -- * Extension Support
    gl_OES_rgb8_rgba8

  -- * GL_OES_rgb8_rgba8
  , pattern GL_RGB8_OES
  , pattern GL_RGBA8_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_rgb8_rgba8.txt GL_OES_rgb8_rgba8> extension is available.

gl_OES_rgb8_rgba8 :: Bool
gl_OES_rgb8_rgba8 = member "GL_OES_rgb8_rgba8" extensions
{-# NOINLINE gl_OES_rgb8_rgba8 #-}