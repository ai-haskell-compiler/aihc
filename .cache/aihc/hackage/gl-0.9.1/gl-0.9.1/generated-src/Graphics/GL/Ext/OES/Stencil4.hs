-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Stencil4 (
  -- * Extension Support
    gl_OES_stencil4

  -- * GL_OES_stencil4
  , pattern GL_STENCIL_INDEX4_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_stencil4.txt GL_OES_stencil4> extension is available.

gl_OES_stencil4 :: Bool
gl_OES_stencil4 = member "GL_OES_stencil4" extensions
{-# NOINLINE gl_OES_stencil4 #-}

pattern GL_STENCIL_INDEX4_OES  :: (Eq a, Num a) => a

pattern GL_STENCIL_INDEX4_OES = 0x8D47