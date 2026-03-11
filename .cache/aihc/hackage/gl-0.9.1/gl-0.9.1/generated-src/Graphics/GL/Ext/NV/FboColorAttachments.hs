-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FboColorAttachments (
  -- * Extension Support
    gl_NV_fbo_color_attachments

  -- * GL_NV_fbo_color_attachments
  , pattern GL_COLOR_ATTACHMENT0_NV
  , pattern GL_COLOR_ATTACHMENT10_NV
  , pattern GL_COLOR_ATTACHMENT11_NV
  , pattern GL_COLOR_ATTACHMENT12_NV
  , pattern GL_COLOR_ATTACHMENT13_NV
  , pattern GL_COLOR_ATTACHMENT14_NV
  , pattern GL_COLOR_ATTACHMENT15_NV
  , pattern GL_COLOR_ATTACHMENT1_NV
  , pattern GL_COLOR_ATTACHMENT2_NV
  , pattern GL_COLOR_ATTACHMENT3_NV
  , pattern GL_COLOR_ATTACHMENT4_NV
  , pattern GL_COLOR_ATTACHMENT5_NV
  , pattern GL_COLOR_ATTACHMENT6_NV
  , pattern GL_COLOR_ATTACHMENT7_NV
  , pattern GL_COLOR_ATTACHMENT8_NV
  , pattern GL_COLOR_ATTACHMENT9_NV
  , pattern GL_MAX_COLOR_ATTACHMENTS_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_fbo_color_attachments.txt GL_NV_fbo_color_attachments> extension is available.

gl_NV_fbo_color_attachments :: Bool
gl_NV_fbo_color_attachments = member "GL_NV_fbo_color_attachments" extensions
{-# NOINLINE gl_NV_fbo_color_attachments #-}

pattern GL_MAX_COLOR_ATTACHMENTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_COLOR_ATTACHMENTS_NV = 0x8CDF