-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Stencil1 (
  -- * Extension Support
    gl_OES_stencil1

  -- * GL_OES_stencil1
  , pattern GL_STENCIL_INDEX1_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_stencil1.txt GL_OES_stencil1> extension is available.

gl_OES_stencil1 :: Bool
gl_OES_stencil1 = member "GL_OES_stencil1" extensions
{-# NOINLINE gl_OES_stencil1 #-}

pattern GL_STENCIL_INDEX1_OES  :: (Eq a, Num a) => a

pattern GL_STENCIL_INDEX1_OES = 0x8D46