-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderImageLoadFormatted (
  -- * Extension Support
    gl_EXT_shader_image_load_formatted
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/shader_image_load_formatted.txt GL_EXT_shader_image_load_formatted> extension is available.

gl_EXT_shader_image_load_formatted :: Bool
gl_EXT_shader_image_load_formatted = member "GL_EXT_shader_image_load_formatted" extensions
{-# NOINLINE gl_EXT_shader_image_load_formatted #-}