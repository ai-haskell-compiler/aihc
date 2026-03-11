-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.Texture2DLimitedNpot (
  -- * Extension Support
    gl_APPLE_texture_2D_limited_npot
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_texture_2D_limited_npot.txt GL_APPLE_texture_2D_limited_npot> extension is available.

gl_APPLE_texture_2D_limited_npot :: Bool
gl_APPLE_texture_2D_limited_npot = member "GL_APPLE_texture_2D_limited_npot" extensions
{-# NOINLINE gl_APPLE_texture_2D_limited_npot #-}