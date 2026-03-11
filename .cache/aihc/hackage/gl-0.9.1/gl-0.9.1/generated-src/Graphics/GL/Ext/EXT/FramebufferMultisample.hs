-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FramebufferMultisample (
  -- * Extension Support
    gl_EXT_framebuffer_multisample

  -- * GL_EXT_framebuffer_multisample
  , glRenderbufferStorageMultisampleEXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT
  , pattern GL_MAX_SAMPLES_EXT
  , pattern GL_RENDERBUFFER_SAMPLES_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/framebuffer_multisample.txt GL_EXT_framebuffer_multisample> extension is available.

gl_EXT_framebuffer_multisample :: Bool
gl_EXT_framebuffer_multisample = member "GL_EXT_framebuffer_multisample" extensions
{-# NOINLINE gl_EXT_framebuffer_multisample #-}