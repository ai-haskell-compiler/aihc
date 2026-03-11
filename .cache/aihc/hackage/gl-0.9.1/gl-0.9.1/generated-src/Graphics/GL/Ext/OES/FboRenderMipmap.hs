-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.FboRenderMipmap (
  -- * Extension Support
    gl_OES_fbo_render_mipmap
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_fbo_render_mipmap.txt GL_OES_fbo_render_mipmap> extension is available.

gl_OES_fbo_render_mipmap :: Bool
gl_OES_fbo_render_mipmap = member "GL_OES_fbo_render_mipmap" extensions
{-# NOINLINE gl_OES_fbo_render_mipmap #-}