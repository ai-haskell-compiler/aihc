-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.TextureLod (
  -- * Extension Support
    gl_SGIS_texture_lod

  -- * GL_SGIS_texture_lod
  , pattern GL_TEXTURE_BASE_LEVEL_SGIS
  , pattern GL_TEXTURE_MAX_LEVEL_SGIS
  , pattern GL_TEXTURE_MAX_LOD_SGIS
  , pattern GL_TEXTURE_MIN_LOD_SGIS
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/texture_lod.txt GL_SGIS_texture_lod> extension is available.

gl_SGIS_texture_lod :: Bool
gl_SGIS_texture_lod = member "GL_SGIS_texture_lod" extensions
{-# NOINLINE gl_SGIS_texture_lod #-}

pattern GL_TEXTURE_BASE_LEVEL_SGIS  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BASE_LEVEL_SGIS = 0x813C

pattern GL_TEXTURE_MAX_LEVEL_SGIS  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAX_LEVEL_SGIS = 0x813D

pattern GL_TEXTURE_MAX_LOD_SGIS  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAX_LOD_SGIS = 0x813B

pattern GL_TEXTURE_MIN_LOD_SGIS  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MIN_LOD_SGIS = 0x813A