-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARM.ShaderFramebufferFetch (
  -- * Extension Support
    gl_ARM_shader_framebuffer_fetch

  -- * GL_ARM_shader_framebuffer_fetch
  , pattern GL_FETCH_PER_SAMPLE_ARM
  , pattern GL_FRAGMENT_SHADER_FRAMEBUFFER_FETCH_MRT_ARM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ARM/ARM_shader_framebuffer_fetch.txt GL_ARM_shader_framebuffer_fetch> extension is available.

gl_ARM_shader_framebuffer_fetch :: Bool
gl_ARM_shader_framebuffer_fetch = member "GL_ARM_shader_framebuffer_fetch" extensions
{-# NOINLINE gl_ARM_shader_framebuffer_fetch #-}

pattern GL_FETCH_PER_SAMPLE_ARM  :: (Eq a, Num a) => a

pattern GL_FETCH_PER_SAMPLE_ARM = 0x8F65

pattern GL_FRAGMENT_SHADER_FRAMEBUFFER_FETCH_MRT_ARM  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_FRAMEBUFFER_FETCH_MRT_ARM = 0x8F66