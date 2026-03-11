-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureEnvCrossbar (
  -- * Extension Support
    gl_OES_texture_env_crossbar
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_env_crossbar.txt GL_OES_texture_env_crossbar> extension is available.

gl_OES_texture_env_crossbar :: Bool
gl_OES_texture_env_crossbar = member "GL_OES_texture_env_crossbar" extensions
{-# NOINLINE gl_OES_texture_env_crossbar #-}