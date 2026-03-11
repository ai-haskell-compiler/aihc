-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.TextureScaleBias (
  -- * Extension Support
    gl_SGIX_texture_scale_bias

  -- * GL_SGIX_texture_scale_bias
  , pattern GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX
  , pattern GL_POST_TEXTURE_FILTER_BIAS_SGIX
  , pattern GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX
  , pattern GL_POST_TEXTURE_FILTER_SCALE_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/texture_scale_bias.txt GL_SGIX_texture_scale_bias> extension is available.

gl_SGIX_texture_scale_bias :: Bool
gl_SGIX_texture_scale_bias = member "GL_SGIX_texture_scale_bias" extensions
{-# NOINLINE gl_SGIX_texture_scale_bias #-}

pattern GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX  :: (Eq a, Num a) => a

pattern GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX = 0x817B

pattern GL_POST_TEXTURE_FILTER_BIAS_SGIX  :: (Eq a, Num a) => a

pattern GL_POST_TEXTURE_FILTER_BIAS_SGIX = 0x8179

pattern GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX  :: (Eq a, Num a) => a

pattern GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX = 0x817C

pattern GL_POST_TEXTURE_FILTER_SCALE_SGIX  :: (Eq a, Num a) => a

pattern GL_POST_TEXTURE_FILTER_SCALE_SGIX = 0x817A