-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureSwizzle (
  -- * Extension Support
    gl_ARB_texture_swizzle

  -- * GL_ARB_texture_swizzle
  , pattern GL_TEXTURE_SWIZZLE_A
  , pattern GL_TEXTURE_SWIZZLE_B
  , pattern GL_TEXTURE_SWIZZLE_G
  , pattern GL_TEXTURE_SWIZZLE_R
  , pattern GL_TEXTURE_SWIZZLE_RGBA
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_swizzle.txt GL_ARB_texture_swizzle> extension is available.

gl_ARB_texture_swizzle :: Bool
gl_ARB_texture_swizzle = member "GL_ARB_texture_swizzle" extensions
{-# NOINLINE gl_ARB_texture_swizzle #-}