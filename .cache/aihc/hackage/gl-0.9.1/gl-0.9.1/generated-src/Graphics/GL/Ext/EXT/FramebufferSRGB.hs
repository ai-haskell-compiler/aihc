-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FramebufferSRGB (
  -- * Extension Support
    gl_EXT_framebuffer_sRGB

  -- * GL_EXT_framebuffer_sRGB
  , pattern GL_FRAMEBUFFER_SRGB_CAPABLE_EXT
  , pattern GL_FRAMEBUFFER_SRGB_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/framebuffer_sRGB.txt GL_EXT_framebuffer_sRGB> extension is available.

gl_EXT_framebuffer_sRGB :: Bool
gl_EXT_framebuffer_sRGB = member "GL_EXT_framebuffer_sRGB" extensions
{-# NOINLINE gl_EXT_framebuffer_sRGB #-}

pattern GL_FRAMEBUFFER_SRGB_CAPABLE_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_SRGB_CAPABLE_EXT = 0x8DBA