-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FramebufferMultisampleBlitScaled (
  -- * Extension Support
    gl_EXT_framebuffer_multisample_blit_scaled

  -- * GL_EXT_framebuffer_multisample_blit_scaled
  , pattern GL_SCALED_RESOLVE_FASTEST_EXT
  , pattern GL_SCALED_RESOLVE_NICEST_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/framebuffer_multisample_blit_scaled.txt GL_EXT_framebuffer_multisample_blit_scaled> extension is available.

gl_EXT_framebuffer_multisample_blit_scaled :: Bool
gl_EXT_framebuffer_multisample_blit_scaled = member "GL_EXT_framebuffer_multisample_blit_scaled" extensions
{-# NOINLINE gl_EXT_framebuffer_multisample_blit_scaled #-}

pattern GL_SCALED_RESOLVE_FASTEST_EXT  :: (Eq a, Num a) => a

pattern GL_SCALED_RESOLVE_FASTEST_EXT = 0x90BA

pattern GL_SCALED_RESOLVE_NICEST_EXT  :: (Eq a, Num a) => a

pattern GL_SCALED_RESOLVE_NICEST_EXT = 0x90BB