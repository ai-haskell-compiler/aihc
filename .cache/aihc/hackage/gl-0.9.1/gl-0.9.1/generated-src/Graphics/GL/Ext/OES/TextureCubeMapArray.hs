-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureCubeMapArray (
  -- * Extension Support
    gl_OES_texture_cube_map_array

  -- * GL_OES_texture_cube_map_array
  , pattern GL_IMAGE_CUBE_MAP_ARRAY_OES
  , pattern GL_INT_IMAGE_CUBE_MAP_ARRAY_OES
  , pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_OES
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_OES
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_OES
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_OES
  , pattern GL_TEXTURE_CUBE_MAP_ARRAY_OES
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY_OES
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_cube_map_array.txt GL_OES_texture_cube_map_array> extension is available.

gl_OES_texture_cube_map_array :: Bool
gl_OES_texture_cube_map_array = member "GL_OES_texture_cube_map_array" extensions
{-# NOINLINE gl_OES_texture_cube_map_array #-}

pattern GL_IMAGE_CUBE_MAP_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_IMAGE_CUBE_MAP_ARRAY_OES = 0x9054

pattern GL_INT_IMAGE_CUBE_MAP_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_INT_IMAGE_CUBE_MAP_ARRAY_OES = 0x905F

pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_OES = 0x900E

pattern GL_SAMPLER_CUBE_MAP_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_MAP_ARRAY_OES = 0x900C

pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_OES  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_OES = 0x900D

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_OES = 0x900A

pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY_OES = 0x906A

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_OES = 0x900F