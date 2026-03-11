-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.StencilWrap (
  -- * Extension Support
    gl_EXT_stencil_wrap

  -- * GL_EXT_stencil_wrap
  , pattern GL_DECR_WRAP_EXT
  , pattern GL_INCR_WRAP_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/stencil_wrap.txt GL_EXT_stencil_wrap> extension is available.

gl_EXT_stencil_wrap :: Bool
gl_EXT_stencil_wrap = member "GL_EXT_stencil_wrap" extensions
{-# NOINLINE gl_EXT_stencil_wrap #-}

pattern GL_DECR_WRAP_EXT  :: (Eq a, Num a) => a

pattern GL_DECR_WRAP_EXT = 0x8508

pattern GL_INCR_WRAP_EXT  :: (Eq a, Num a) => a

pattern GL_INCR_WRAP_EXT = 0x8507