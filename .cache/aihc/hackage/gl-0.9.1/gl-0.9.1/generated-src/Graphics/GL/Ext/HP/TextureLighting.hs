-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.HP.TextureLighting (
  -- * Extension Support
    gl_HP_texture_lighting

  -- * GL_HP_texture_lighting
  , pattern GL_TEXTURE_LIGHTING_MODE_HP
  , pattern GL_TEXTURE_POST_SPECULAR_HP
  , pattern GL_TEXTURE_PRE_SPECULAR_HP
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/HP/texture_lighting.txt GL_HP_texture_lighting> extension is available.

gl_HP_texture_lighting :: Bool
gl_HP_texture_lighting = member "GL_HP_texture_lighting" extensions
{-# NOINLINE gl_HP_texture_lighting #-}

pattern GL_TEXTURE_LIGHTING_MODE_HP  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LIGHTING_MODE_HP = 0x8167

pattern GL_TEXTURE_POST_SPECULAR_HP  :: (Eq a, Num a) => a

pattern GL_TEXTURE_POST_SPECULAR_HP = 0x8168

pattern GL_TEXTURE_PRE_SPECULAR_HP  :: (Eq a, Num a) => a

pattern GL_TEXTURE_PRE_SPECULAR_HP = 0x8169