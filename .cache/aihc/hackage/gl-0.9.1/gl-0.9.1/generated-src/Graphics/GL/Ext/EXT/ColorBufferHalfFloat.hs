-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ColorBufferHalfFloat (
  -- * Extension Support
    gl_EXT_color_buffer_half_float

  -- * GL_EXT_color_buffer_half_float
  , pattern GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT
  , pattern GL_R16F_EXT
  , pattern GL_RG16F_EXT
  , pattern GL_RGB16F_EXT
  , pattern GL_RGBA16F_EXT
  , pattern GL_UNSIGNED_NORMALIZED_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_color_buffer_half_float.txt GL_EXT_color_buffer_half_float> extension is available.

gl_EXT_color_buffer_half_float :: Bool
gl_EXT_color_buffer_half_float = member "GL_EXT_color_buffer_half_float" extensions
{-# NOINLINE gl_EXT_color_buffer_half_float #-}

pattern GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT = 0x8211

pattern GL_UNSIGNED_NORMALIZED_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_NORMALIZED_EXT = 0x8C17