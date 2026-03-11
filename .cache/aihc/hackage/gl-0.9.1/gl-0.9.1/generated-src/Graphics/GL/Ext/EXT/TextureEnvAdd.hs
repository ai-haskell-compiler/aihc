-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureEnvAdd (
  -- * Extension Support
    gl_EXT_texture_env_add
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_env_add.txt GL_EXT_texture_env_add> extension is available.

gl_EXT_texture_env_add :: Bool
gl_EXT_texture_env_add = member "GL_EXT_texture_env_add" extensions
{-# NOINLINE gl_EXT_texture_env_add #-}