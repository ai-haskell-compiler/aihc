-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.TextureCoordinateClamp (
  -- * Extension Support
    gl_SGIX_texture_coordinate_clamp

  -- * GL_SGIX_texture_coordinate_clamp
  , pattern GL_TEXTURE_MAX_CLAMP_R_SGIX
  , pattern GL_TEXTURE_MAX_CLAMP_S_SGIX
  , pattern GL_TEXTURE_MAX_CLAMP_T_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/texture_coordinate_clamp.txt GL_SGIX_texture_coordinate_clamp> extension is available.

gl_SGIX_texture_coordinate_clamp :: Bool
gl_SGIX_texture_coordinate_clamp = member "GL_SGIX_texture_coordinate_clamp" extensions
{-# NOINLINE gl_SGIX_texture_coordinate_clamp #-}

pattern GL_TEXTURE_MAX_CLAMP_R_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAX_CLAMP_R_SGIX = 0x836B

pattern GL_TEXTURE_MAX_CLAMP_S_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAX_CLAMP_S_SGIX = 0x8369

pattern GL_TEXTURE_MAX_CLAMP_T_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAX_CLAMP_T_SGIX = 0x836A