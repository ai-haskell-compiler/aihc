-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.ShadowAmbient (
  -- * Extension Support
    gl_SGIX_shadow_ambient

  -- * GL_SGIX_shadow_ambient
  , pattern GL_SHADOW_AMBIENT_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/shadow_ambient.txt GL_SGIX_shadow_ambient> extension is available.

gl_SGIX_shadow_ambient :: Bool
gl_SGIX_shadow_ambient = member "GL_SGIX_shadow_ambient" extensions
{-# NOINLINE gl_SGIX_shadow_ambient #-}

pattern GL_SHADOW_AMBIENT_SGIX  :: (Eq a, Num a) => a

pattern GL_SHADOW_AMBIENT_SGIX = 0x80BF