-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureLodBias (
  -- * Extension Support
    gl_EXT_texture_lod_bias

  -- * GL_EXT_texture_lod_bias
  , pattern GL_MAX_TEXTURE_LOD_BIAS_EXT
  , pattern GL_TEXTURE_FILTER_CONTROL_EXT
  , pattern GL_TEXTURE_LOD_BIAS_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_lod_bias.txt GL_EXT_texture_lod_bias> extension is available.

gl_EXT_texture_lod_bias :: Bool
gl_EXT_texture_lod_bias = member "GL_EXT_texture_lod_bias" extensions
{-# NOINLINE gl_EXT_texture_lod_bias #-}

pattern GL_MAX_TEXTURE_LOD_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_TEXTURE_LOD_BIAS_EXT = 0x84FD

pattern GL_TEXTURE_FILTER_CONTROL_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_FILTER_CONTROL_EXT = 0x8500

pattern GL_TEXTURE_LOD_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LOD_BIAS_EXT = 0x8501