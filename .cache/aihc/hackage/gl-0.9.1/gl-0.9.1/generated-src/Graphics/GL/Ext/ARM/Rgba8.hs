-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARM.Rgba8 (
  -- * Extension Support
    gl_ARM_rgba8
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ARM/ARM_rgba8.txt GL_ARM_rgba8> extension is available.

gl_ARM_rgba8 :: Bool
gl_ARM_rgba8 = member "GL_ARM_rgba8" extensions
{-# NOINLINE gl_ARM_rgba8 #-}