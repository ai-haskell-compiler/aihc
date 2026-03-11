-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ShaderImageAtomic (
  -- * Extension Support
    gl_OES_shader_image_atomic
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_shader_image_atomic.txt GL_OES_shader_image_atomic> extension is available.

gl_OES_shader_image_atomic :: Bool
gl_OES_shader_image_atomic = member "GL_OES_shader_image_atomic" extensions
{-# NOINLINE gl_OES_shader_image_atomic #-}