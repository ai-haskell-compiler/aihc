-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureCubeMap (
  -- * Extension Support
    gl_ARB_texture_cube_map

  -- * GL_ARB_texture_cube_map
  , pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB
  , pattern GL_NORMAL_MAP_ARB
  , pattern GL_PROXY_TEXTURE_CUBE_MAP_ARB
  , pattern GL_REFLECTION_MAP_ARB
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_ARB
  , pattern GL_TEXTURE_CUBE_MAP_ARB
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_cube_map.txt GL_ARB_texture_cube_map> extension is available.

gl_ARB_texture_cube_map :: Bool
gl_ARB_texture_cube_map = member "GL_ARB_texture_cube_map" extensions
{-# NOINLINE gl_ARB_texture_cube_map #-}

pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = 0x851C

pattern GL_NORMAL_MAP_ARB  :: (Eq a, Num a) => a

pattern GL_NORMAL_MAP_ARB = 0x8511

pattern GL_PROXY_TEXTURE_CUBE_MAP_ARB  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_CUBE_MAP_ARB = 0x851B

pattern GL_REFLECTION_MAP_ARB  :: (Eq a, Num a) => a

pattern GL_REFLECTION_MAP_ARB = 0x8512

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARB = 0x8514

pattern GL_TEXTURE_CUBE_MAP_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_ARB = 0x8513

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = 0x8516

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = 0x8518

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = 0x851A

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = 0x8515

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = 0x8517

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = 0x8519