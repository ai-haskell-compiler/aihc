-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderIoBlocks (
  -- * Extension Support
    gl_EXT_shader_io_blocks
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_shader_io_blocks.txt GL_EXT_shader_io_blocks> extension is available.

gl_EXT_shader_io_blocks :: Bool
gl_EXT_shader_io_blocks = member "GL_EXT_shader_io_blocks" extensions
{-# NOINLINE gl_EXT_shader_io_blocks #-}