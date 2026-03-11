-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ReadDepth (
  -- * Extension Support
    gl_NV_read_depth
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_NV_read_depth extension is available.

gl_NV_read_depth :: Bool
gl_NV_read_depth = member "GL_NV_read_depth" extensions
{-# NOINLINE gl_NV_read_depth #-}