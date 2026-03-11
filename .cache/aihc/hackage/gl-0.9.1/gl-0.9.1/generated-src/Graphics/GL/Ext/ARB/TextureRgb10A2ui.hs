-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureRgb10A2ui (
  -- * Extension Support
    gl_ARB_texture_rgb10_a2ui

  -- * GL_ARB_texture_rgb10_a2ui
  , pattern GL_RGB10_A2UI
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_rgb10_a2ui.txt GL_ARB_texture_rgb10_a2ui> extension is available.

gl_ARB_texture_rgb10_a2ui :: Bool
gl_ARB_texture_rgb10_a2ui = member "GL_ARB_texture_rgb10_a2ui" extensions
{-# NOINLINE gl_ARB_texture_rgb10_a2ui #-}