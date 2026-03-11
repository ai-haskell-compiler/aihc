-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureBufferObjectRgb32 (
  -- * Extension Support
    gl_ARB_texture_buffer_object_rgb32

  -- * GL_ARB_texture_buffer_object_rgb32
  , pattern GL_RGB32F
  , pattern GL_RGB32I
  , pattern GL_RGB32UI
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_buffer_object_rgb32.txt GL_ARB_texture_buffer_object_rgb32> extension is available.

gl_ARB_texture_buffer_object_rgb32 :: Bool
gl_ARB_texture_buffer_object_rgb32 = member "GL_ARB_texture_buffer_object_rgb32" extensions
{-# NOINLINE gl_ARB_texture_buffer_object_rgb32 #-}