-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureBufferRange (
  -- * Extension Support
    gl_ARB_texture_buffer_range

  -- * GL_ARB_texture_buffer_range
  , glTexBufferRange
  , pattern GL_TEXTURE_BUFFER_OFFSET
  , pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT
  , pattern GL_TEXTURE_BUFFER_SIZE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_buffer_range.txt GL_ARB_texture_buffer_range> extension is available.

gl_ARB_texture_buffer_range :: Bool
gl_ARB_texture_buffer_range = member "GL_ARB_texture_buffer_range" extensions
{-# NOINLINE gl_ARB_texture_buffer_range #-}