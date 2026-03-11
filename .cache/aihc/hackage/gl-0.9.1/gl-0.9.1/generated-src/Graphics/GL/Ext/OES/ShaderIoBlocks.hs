-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ShaderIoBlocks (
  -- * Extension Support
    gl_OES_shader_io_blocks
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_shader_io_blocks.txt GL_OES_shader_io_blocks> extension is available.

gl_OES_shader_io_blocks :: Bool
gl_OES_shader_io_blocks = member "GL_OES_shader_io_blocks" extensions
{-# NOINLINE gl_OES_shader_io_blocks #-}