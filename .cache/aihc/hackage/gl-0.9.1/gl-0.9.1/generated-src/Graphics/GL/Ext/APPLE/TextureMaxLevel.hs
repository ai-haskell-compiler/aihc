-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.TextureMaxLevel (
  -- * Extension Support
    gl_APPLE_texture_max_level

  -- * GL_APPLE_texture_max_level
  , pattern GL_TEXTURE_MAX_LEVEL_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_texture_max_level.txt GL_APPLE_texture_max_level> extension is available.

gl_APPLE_texture_max_level :: Bool
gl_APPLE_texture_max_level = member "GL_APPLE_texture_max_level" extensions
{-# NOINLINE gl_APPLE_texture_max_level #-}

pattern GL_TEXTURE_MAX_LEVEL_APPLE  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAX_LEVEL_APPLE = 0x813D