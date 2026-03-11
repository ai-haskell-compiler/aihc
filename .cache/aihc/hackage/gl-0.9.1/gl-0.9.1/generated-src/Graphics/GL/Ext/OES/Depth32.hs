-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Depth32 (
  -- * Extension Support
    gl_OES_depth32

  -- * GL_OES_depth32
  , pattern GL_DEPTH_COMPONENT32_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_depth32.txt GL_OES_depth32> extension is available.

gl_OES_depth32 :: Bool
gl_OES_depth32 = member "GL_OES_depth32" extensions
{-# NOINLINE gl_OES_depth32 #-}