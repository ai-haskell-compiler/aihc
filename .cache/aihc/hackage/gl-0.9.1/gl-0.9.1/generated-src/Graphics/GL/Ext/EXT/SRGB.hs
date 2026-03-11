-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SRGB (
  -- * Extension Support
    gl_EXT_sRGB

  -- * GL_EXT_sRGB
  , pattern GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT
  , pattern GL_SRGB8_ALPHA8_EXT
  , pattern GL_SRGB_ALPHA_EXT
  , pattern GL_SRGB_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_sRGB.txt GL_EXT_sRGB> extension is available.

gl_EXT_sRGB :: Bool
gl_EXT_sRGB = member "GL_EXT_sRGB" extensions
{-# NOINLINE gl_EXT_sRGB #-}

pattern GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT = 0x8210