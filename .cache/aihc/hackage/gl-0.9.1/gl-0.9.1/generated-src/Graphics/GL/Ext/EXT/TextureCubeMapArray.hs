-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureCubeMapArray (
  -- * Extension Support
    gl_EXT_texture_cube_map_array

  -- * GL_EXT_texture_cube_map_array
  , pattern GL_IMAGE_CUBE_MAP_ARRAY_EXT
  , pattern GL_INT_IMAGE_CUBE_MAP_ARRAY_EXT
  , pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_EXT
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_EXT
  , pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_EXT
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_EXT
  , pattern GL_TEXTURE_CUBE_MAP_ARRAY_EXT
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_cube_map_array.txt GL_EXT_texture_cube_map_array> extension is available.

gl_EXT_texture_cube_map_array :: Bool
gl_EXT_texture_cube_map_array = member "GL_EXT_texture_cube_map_array" extensions
{-# NOINLINE gl_EXT_texture_cube_map_array #-}

pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_CUBE_MAP_ARRAY_EXT = 0x900E

pattern GL_SAMPLER_CUBE_MAP_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_MAP_ARRAY_EXT = 0x900C

pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_EXT = 0x900D

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_EXT = 0x900A

pattern GL_TEXTURE_CUBE_MAP_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_ARRAY_EXT = 0x9009

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_EXT = 0x900F