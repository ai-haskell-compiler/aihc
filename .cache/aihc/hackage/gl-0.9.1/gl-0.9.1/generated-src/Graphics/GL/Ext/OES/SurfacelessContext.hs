-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.SurfacelessContext (
  -- * Extension Support
    gl_OES_surfaceless_context

  -- * GL_OES_surfaceless_context
  , pattern GL_FRAMEBUFFER_UNDEFINED_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_surfaceless_context.txt GL_OES_surfaceless_context> extension is available.

gl_OES_surfaceless_context :: Bool
gl_OES_surfaceless_context = member "GL_OES_surfaceless_context" extensions
{-# NOINLINE gl_OES_surfaceless_context #-}

pattern GL_FRAMEBUFFER_UNDEFINED_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_UNDEFINED_OES = 0x8219