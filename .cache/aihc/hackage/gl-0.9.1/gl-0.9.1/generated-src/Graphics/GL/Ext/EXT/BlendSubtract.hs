-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BlendSubtract (
  -- * Extension Support
    gl_EXT_blend_subtract

  -- * GL_EXT_blend_subtract
  , pattern GL_FUNC_REVERSE_SUBTRACT_EXT
  , pattern GL_FUNC_SUBTRACT_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/blend_subtract.txt GL_EXT_blend_subtract> extension is available.

gl_EXT_blend_subtract :: Bool
gl_EXT_blend_subtract = member "GL_EXT_blend_subtract" extensions
{-# NOINLINE gl_EXT_blend_subtract #-}

pattern GL_FUNC_REVERSE_SUBTRACT_EXT  :: (Eq a, Num a) => a

pattern GL_FUNC_REVERSE_SUBTRACT_EXT = 0x800B

pattern GL_FUNC_SUBTRACT_EXT  :: (Eq a, Num a) => a

pattern GL_FUNC_SUBTRACT_EXT = 0x800A