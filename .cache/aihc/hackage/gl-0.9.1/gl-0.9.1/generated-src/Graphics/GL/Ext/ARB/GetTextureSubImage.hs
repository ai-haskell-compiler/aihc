-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.GetTextureSubImage (
  -- * Extension Support
    gl_ARB_get_texture_sub_image

  -- * GL_ARB_get_texture_sub_image
  , glGetCompressedTextureSubImage
  , glGetTextureSubImage
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/get_texture_sub_image.txt GL_ARB_get_texture_sub_image> extension is available.

gl_ARB_get_texture_sub_image :: Bool
gl_ARB_get_texture_sub_image = member "GL_ARB_get_texture_sub_image" extensions
{-# NOINLINE gl_ARB_get_texture_sub_image #-}