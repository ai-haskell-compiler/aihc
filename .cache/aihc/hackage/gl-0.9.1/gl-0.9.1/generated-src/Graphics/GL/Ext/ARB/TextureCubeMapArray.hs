-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureCubeMapArray (
  -- * Extension Support
    gl_ARB_texture_cube_map_array

  -- * GL_ARB_texture_cube_map_array
  , pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB
  , pattern GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_ARB
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB
  , pattern GL_TEXTURE_CUBE_MAP_ARRAY_ARB
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_cube_map_array.txt GL_ARB_texture_cube_map_array> extension is available.

gl_ARB_texture_cube_map_array :: Bool
gl_ARB_texture_cube_map_array = member "GL_ARB_texture_cube_map_array" extensions
{-# NOINLINE gl_ARB_texture_cube_map_array #-}

pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB = 0x900E

pattern GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB = 0x900B

pattern GL_SAMPLER_CUBE_MAP_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_MAP_ARRAY_ARB = 0x900C

pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB = 0x900D

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB = 0x900A

pattern GL_TEXTURE_CUBE_MAP_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_ARRAY_ARB = 0x9009

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB = 0x900F