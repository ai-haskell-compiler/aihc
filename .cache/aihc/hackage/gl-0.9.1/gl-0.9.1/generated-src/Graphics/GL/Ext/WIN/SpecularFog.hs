-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.WIN.SpecularFog (
  -- * Extension Support
    gl_WIN_specular_fog

  -- * GL_WIN_specular_fog
  , pattern GL_FOG_SPECULAR_TEXTURE_WIN
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/WIN/specular_fog.txt GL_WIN_specular_fog> extension is available.

gl_WIN_specular_fog :: Bool
gl_WIN_specular_fog = member "GL_WIN_specular_fog" extensions
{-# NOINLINE gl_WIN_specular_fog #-}

pattern GL_FOG_SPECULAR_TEXTURE_WIN  :: (Eq a, Num a) => a

pattern GL_FOG_SPECULAR_TEXTURE_WIN = 0x80EC