-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Depth24 (
  -- * Extension Support
    gl_OES_depth24

  -- * GL_OES_depth24
  , pattern GL_DEPTH_COMPONENT24_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_depth24.txt GL_OES_depth24> extension is available.

gl_OES_depth24 :: Bool
gl_OES_depth24 = member "GL_OES_depth24" extensions
{-# NOINLINE gl_OES_depth24 #-}