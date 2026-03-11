-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureStencil8 (
  -- * Extension Support
    gl_OES_texture_stencil8

  -- * GL_OES_texture_stencil8
  , pattern GL_STENCIL_INDEX8_OES
  , pattern GL_STENCIL_INDEX_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_stencil8.txt GL_OES_texture_stencil8> extension is available.

gl_OES_texture_stencil8 :: Bool
gl_OES_texture_stencil8 = member "GL_OES_texture_stencil8" extensions
{-# NOINLINE gl_OES_texture_stencil8 #-}

pattern GL_STENCIL_INDEX_OES  :: (Eq a, Num a) => a

pattern GL_STENCIL_INDEX_OES = 0x1901