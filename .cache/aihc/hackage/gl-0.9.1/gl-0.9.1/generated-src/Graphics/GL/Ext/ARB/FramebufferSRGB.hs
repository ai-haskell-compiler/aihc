-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.FramebufferSRGB (
  -- * Extension Support
    gl_ARB_framebuffer_sRGB

  -- * GL_ARB_framebuffer_sRGB
  , pattern GL_FRAMEBUFFER_SRGB
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/framebuffer_sRGB.txt GL_ARB_framebuffer_sRGB> extension is available.

gl_ARB_framebuffer_sRGB :: Bool
gl_ARB_framebuffer_sRGB = member "GL_ARB_framebuffer_sRGB" extensions
{-# NOINLINE gl_ARB_framebuffer_sRGB #-}