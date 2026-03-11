-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.StencilWrap (
  -- * Extension Support
    gl_OES_stencil_wrap

  -- * GL_OES_stencil_wrap
  , pattern GL_DECR_WRAP_OES
  , pattern GL_INCR_WRAP_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_stencil_wrap.txt GL_OES_stencil_wrap> extension is available.

gl_OES_stencil_wrap :: Bool
gl_OES_stencil_wrap = member "GL_OES_stencil_wrap" extensions
{-# NOINLINE gl_OES_stencil_wrap #-}

pattern GL_DECR_WRAP_OES  :: (Eq a, Num a) => a

pattern GL_DECR_WRAP_OES = 0x8508

pattern GL_INCR_WRAP_OES  :: (Eq a, Num a) => a

pattern GL_INCR_WRAP_OES = 0x8507