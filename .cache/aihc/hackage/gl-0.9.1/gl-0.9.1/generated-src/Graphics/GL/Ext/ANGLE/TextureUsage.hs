-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.TextureUsage (
  -- * Extension Support
    gl_ANGLE_texture_usage

  -- * GL_ANGLE_texture_usage
  , pattern GL_FRAMEBUFFER_ATTACHMENT_ANGLE
  , pattern GL_TEXTURE_USAGE_ANGLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_texture_usage.txt GL_ANGLE_texture_usage> extension is available.

gl_ANGLE_texture_usage :: Bool
gl_ANGLE_texture_usage = member "GL_ANGLE_texture_usage" extensions
{-# NOINLINE gl_ANGLE_texture_usage #-}

pattern GL_FRAMEBUFFER_ATTACHMENT_ANGLE  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_ANGLE = 0x93A3

pattern GL_TEXTURE_USAGE_ANGLE  :: (Eq a, Num a) => a

pattern GL_TEXTURE_USAGE_ANGLE = 0x93A2