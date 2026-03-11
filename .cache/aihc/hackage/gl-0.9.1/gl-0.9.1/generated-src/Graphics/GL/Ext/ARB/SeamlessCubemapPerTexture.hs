-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SeamlessCubemapPerTexture (
  -- * Extension Support
    gl_ARB_seamless_cubemap_per_texture

  -- * GL_ARB_seamless_cubemap_per_texture
  , pattern GL_TEXTURE_CUBE_MAP_SEAMLESS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/seamless_cubemap_per_texture.txt GL_ARB_seamless_cubemap_per_texture> extension is available.

gl_ARB_seamless_cubemap_per_texture :: Bool
gl_ARB_seamless_cubemap_per_texture = member "GL_ARB_seamless_cubemap_per_texture" extensions
{-# NOINLINE gl_ARB_seamless_cubemap_per_texture #-}