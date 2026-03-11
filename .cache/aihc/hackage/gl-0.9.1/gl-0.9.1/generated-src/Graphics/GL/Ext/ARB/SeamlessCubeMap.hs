-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SeamlessCubeMap (
  -- * Extension Support
    gl_ARB_seamless_cube_map

  -- * GL_ARB_seamless_cube_map
  , pattern GL_TEXTURE_CUBE_MAP_SEAMLESS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/seamless_cube_map.txt GL_ARB_seamless_cube_map> extension is available.

gl_ARB_seamless_cube_map :: Bool
gl_ARB_seamless_cube_map = member "GL_ARB_seamless_cube_map" extensions
{-# NOINLINE gl_ARB_seamless_cube_map #-}