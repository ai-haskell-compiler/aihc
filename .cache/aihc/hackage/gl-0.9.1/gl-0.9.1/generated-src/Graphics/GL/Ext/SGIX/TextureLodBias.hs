-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.TextureLodBias (
  -- * Extension Support
    gl_SGIX_texture_lod_bias

  -- * GL_SGIX_texture_lod_bias
  , pattern GL_TEXTURE_LOD_BIAS_R_SGIX
  , pattern GL_TEXTURE_LOD_BIAS_S_SGIX
  , pattern GL_TEXTURE_LOD_BIAS_T_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/texture_lod_bias.txt GL_SGIX_texture_lod_bias> extension is available.

gl_SGIX_texture_lod_bias :: Bool
gl_SGIX_texture_lod_bias = member "GL_SGIX_texture_lod_bias" extensions
{-# NOINLINE gl_SGIX_texture_lod_bias #-}

pattern GL_TEXTURE_LOD_BIAS_R_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LOD_BIAS_R_SGIX = 0x8190

pattern GL_TEXTURE_LOD_BIAS_S_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LOD_BIAS_S_SGIX = 0x818E

pattern GL_TEXTURE_LOD_BIAS_T_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LOD_BIAS_T_SGIX = 0x818F