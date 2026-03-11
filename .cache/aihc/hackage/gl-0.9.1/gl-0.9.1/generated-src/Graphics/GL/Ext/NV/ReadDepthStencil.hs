-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ReadDepthStencil (
  -- * Extension Support
    gl_NV_read_depth_stencil
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_read_depth_stencil.txt GL_NV_read_depth_stencil> extension is available.

gl_NV_read_depth_stencil :: Bool
gl_NV_read_depth_stencil = member "GL_NV_read_depth_stencil" extensions
{-# NOINLINE gl_NV_read_depth_stencil #-}