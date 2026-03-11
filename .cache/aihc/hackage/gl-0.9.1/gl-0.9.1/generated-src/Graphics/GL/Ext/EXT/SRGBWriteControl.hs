-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SRGBWriteControl (
  -- * Extension Support
    gl_EXT_sRGB_write_control

  -- * GL_EXT_sRGB_write_control
  , pattern GL_FRAMEBUFFER_SRGB_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_sRGB_write_control.txt GL_EXT_sRGB_write_control> extension is available.

gl_EXT_sRGB_write_control :: Bool
gl_EXT_sRGB_write_control = member "GL_EXT_sRGB_write_control" extensions
{-# NOINLINE gl_EXT_sRGB_write_control #-}