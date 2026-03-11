-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderFramebufferFetch (
  -- * Extension Support
    gl_EXT_shader_framebuffer_fetch

  -- * GL_EXT_shader_framebuffer_fetch
  , pattern GL_FRAGMENT_SHADER_DISCARDS_SAMPLES_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_shader_framebuffer_fetch.txt GL_EXT_shader_framebuffer_fetch> extension is available.

gl_EXT_shader_framebuffer_fetch :: Bool
gl_EXT_shader_framebuffer_fetch = member "GL_EXT_shader_framebuffer_fetch" extensions
{-# NOINLINE gl_EXT_shader_framebuffer_fetch #-}

pattern GL_FRAGMENT_SHADER_DISCARDS_SAMPLES_EXT  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_DISCARDS_SAMPLES_EXT = 0x8A52