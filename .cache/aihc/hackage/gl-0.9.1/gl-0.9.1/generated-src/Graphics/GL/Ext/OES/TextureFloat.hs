-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureFloat (
  -- * Extension Support
    gl_OES_texture_float

  -- * GL_OES_texture_float
  , pattern GL_FLOAT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_float.txt GL_OES_texture_float> extension is available.

gl_OES_texture_float :: Bool
gl_OES_texture_float = member "GL_OES_texture_float" extensions
{-# NOINLINE gl_OES_texture_float #-}