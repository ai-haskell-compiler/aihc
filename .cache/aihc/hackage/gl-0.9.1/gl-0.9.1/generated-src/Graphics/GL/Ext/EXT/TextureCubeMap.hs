-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureCubeMap (
  -- * Extension Support
    gl_EXT_texture_cube_map

  -- * GL_EXT_texture_cube_map
  , pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT
  , pattern GL_NORMAL_MAP_EXT
  , pattern GL_PROXY_TEXTURE_CUBE_MAP_EXT
  , pattern GL_REFLECTION_MAP_EXT
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_EXT
  , pattern GL_TEXTURE_CUBE_MAP_EXT
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_EXT_texture_cube_map extension is available.

gl_EXT_texture_cube_map :: Bool
gl_EXT_texture_cube_map = member "GL_EXT_texture_cube_map" extensions
{-# NOINLINE gl_EXT_texture_cube_map #-}

pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT = 0x851C

pattern GL_NORMAL_MAP_EXT  :: (Eq a, Num a) => a

pattern GL_NORMAL_MAP_EXT = 0x8511

pattern GL_PROXY_TEXTURE_CUBE_MAP_EXT  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_CUBE_MAP_EXT = 0x851B

pattern GL_REFLECTION_MAP_EXT  :: (Eq a, Num a) => a

pattern GL_REFLECTION_MAP_EXT = 0x8512

pattern GL_TEXTURE_BINDING_CUBE_MAP_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_CUBE_MAP_EXT = 0x8514

pattern GL_TEXTURE_CUBE_MAP_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_EXT = 0x8513

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT = 0x8516

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT = 0x8518

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT = 0x851A

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT = 0x8515

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT = 0x8517

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT = 0x8519