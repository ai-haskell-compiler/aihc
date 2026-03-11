-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.FramebufferNoAttachments (
  -- * Extension Support
    gl_ARB_framebuffer_no_attachments

  -- * GL_ARB_framebuffer_no_attachments
  , glFramebufferParameteri
  , glGetFramebufferParameteriv
  , pattern GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS
  , pattern GL_FRAMEBUFFER_DEFAULT_HEIGHT
  , pattern GL_FRAMEBUFFER_DEFAULT_LAYERS
  , pattern GL_FRAMEBUFFER_DEFAULT_SAMPLES
  , pattern GL_FRAMEBUFFER_DEFAULT_WIDTH
  , pattern GL_MAX_FRAMEBUFFER_HEIGHT
  , pattern GL_MAX_FRAMEBUFFER_LAYERS
  , pattern GL_MAX_FRAMEBUFFER_SAMPLES
  , pattern GL_MAX_FRAMEBUFFER_WIDTH
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/framebuffer_no_attachments.txt GL_ARB_framebuffer_no_attachments> extension is available.

gl_ARB_framebuffer_no_attachments :: Bool
gl_ARB_framebuffer_no_attachments = member "GL_ARB_framebuffer_no_attachments" extensions
{-# NOINLINE gl_ARB_framebuffer_no_attachments #-}