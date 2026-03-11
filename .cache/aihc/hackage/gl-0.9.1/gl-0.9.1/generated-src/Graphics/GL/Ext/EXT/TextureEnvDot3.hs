-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureEnvDot3 (
  -- * Extension Support
    gl_EXT_texture_env_dot3

  -- * GL_EXT_texture_env_dot3
  , pattern GL_DOT3_RGBA_EXT
  , pattern GL_DOT3_RGB_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_env_dot3.txt GL_EXT_texture_env_dot3> extension is available.

gl_EXT_texture_env_dot3 :: Bool
gl_EXT_texture_env_dot3 = member "GL_EXT_texture_env_dot3" extensions
{-# NOINLINE gl_EXT_texture_env_dot3 #-}

pattern GL_DOT3_RGBA_EXT  :: (Eq a, Num a) => a

pattern GL_DOT3_RGBA_EXT = 0x8741

pattern GL_DOT3_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_DOT3_RGB_EXT = 0x8740