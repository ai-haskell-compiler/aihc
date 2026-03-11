-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Stencil8 (
  -- * Extension Support
    gl_OES_stencil8

  -- * GL_OES_stencil8
  , pattern GL_STENCIL_INDEX8_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_stencil8.txt GL_OES_stencil8> extension is available.

gl_OES_stencil8 :: Bool
gl_OES_stencil8 = member "GL_OES_stencil8" extensions
{-# NOINLINE gl_OES_stencil8 #-}