-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ConservativeDepth (
  -- * Extension Support
    gl_EXT_conservative_depth
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_conservative_depth.txt GL_EXT_conservative_depth> extension is available.

gl_EXT_conservative_depth :: Bool
gl_EXT_conservative_depth = member "GL_EXT_conservative_depth" extensions
{-# NOINLINE gl_EXT_conservative_depth #-}