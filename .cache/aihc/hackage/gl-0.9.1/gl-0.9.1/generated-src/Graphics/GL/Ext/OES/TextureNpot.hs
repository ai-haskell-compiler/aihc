-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureNpot (
  -- * Extension Support
    gl_OES_texture_npot
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_npot.txt GL_OES_texture_npot> extension is available.

gl_OES_texture_npot :: Bool
gl_OES_texture_npot = member "GL_OES_texture_npot" extensions
{-# NOINLINE gl_OES_texture_npot #-}