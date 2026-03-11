-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ReadStencil (
  -- * Extension Support
    gl_NV_read_stencil
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_NV_read_stencil extension is available.

gl_NV_read_stencil :: Bool
gl_NV_read_stencil = member "GL_NV_read_stencil" extensions
{-# NOINLINE gl_NV_read_stencil #-}