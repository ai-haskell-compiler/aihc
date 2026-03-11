-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureEnvDot3 (
  -- * Extension Support
    gl_ARB_texture_env_dot3

  -- * GL_ARB_texture_env_dot3
  , pattern GL_DOT3_RGBA_ARB
  , pattern GL_DOT3_RGB_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_env_dot3.txt GL_ARB_texture_env_dot3> extension is available.

gl_ARB_texture_env_dot3 :: Bool
gl_ARB_texture_env_dot3 = member "GL_ARB_texture_env_dot3" extensions
{-# NOINLINE gl_ARB_texture_env_dot3 #-}

pattern GL_DOT3_RGBA_ARB  :: (Eq a, Num a) => a

pattern GL_DOT3_RGBA_ARB = 0x86AF

pattern GL_DOT3_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_DOT3_RGB_ARB = 0x86AE