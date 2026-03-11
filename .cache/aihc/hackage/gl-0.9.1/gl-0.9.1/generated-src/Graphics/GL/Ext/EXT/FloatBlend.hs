-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FloatBlend (
  -- * Extension Support
    gl_EXT_float_blend
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_float_blend.txt GL_EXT_float_blend> extension is available.

gl_EXT_float_blend :: Bool
gl_EXT_float_blend = member "GL_EXT_float_blend" extensions
{-# NOINLINE gl_EXT_float_blend #-}