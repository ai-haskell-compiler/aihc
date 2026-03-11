-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARM.ShaderFramebufferFetchDepthStencil (
  -- * Extension Support
    gl_ARM_shader_framebuffer_fetch_depth_stencil
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ARM/ARM_shader_framebuffer_fetch_depth_stencil.txt GL_ARM_shader_framebuffer_fetch_depth_stencil> extension is available.

gl_ARM_shader_framebuffer_fetch_depth_stencil :: Bool
gl_ARM_shader_framebuffer_fetch_depth_stencil = member "GL_ARM_shader_framebuffer_fetch_depth_stencil" extensions
{-# NOINLINE gl_ARM_shader_framebuffer_fetch_depth_stencil #-}