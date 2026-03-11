-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.SeamlessCubemapPerTexture (
  -- * Extension Support
    gl_AMD_seamless_cubemap_per_texture

  -- * GL_AMD_seamless_cubemap_per_texture
  , pattern GL_TEXTURE_CUBE_MAP_SEAMLESS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/seamless_cubemap_per_texture.txt GL_AMD_seamless_cubemap_per_texture> extension is available.

gl_AMD_seamless_cubemap_per_texture :: Bool
gl_AMD_seamless_cubemap_per_texture = member "GL_AMD_seamless_cubemap_per_texture" extensions
{-# NOINLINE gl_AMD_seamless_cubemap_per_texture #-}