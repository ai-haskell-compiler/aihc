-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.TextureAddEnv (
  -- * Extension Support
    gl_SGIX_texture_add_env

  -- * GL_SGIX_texture_add_env
  , pattern GL_TEXTURE_ENV_BIAS_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_texture_add_env extension is available.

gl_SGIX_texture_add_env :: Bool
gl_SGIX_texture_add_env = member "GL_SGIX_texture_add_env" extensions
{-# NOINLINE gl_SGIX_texture_add_env #-}

pattern GL_TEXTURE_ENV_BIAS_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_ENV_BIAS_SGIX = 0x80BE