-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.PackedDepthStencil (
  -- * Extension Support
    gl_OES_packed_depth_stencil

  -- * GL_OES_packed_depth_stencil
  , pattern GL_DEPTH24_STENCIL8_OES
  , pattern GL_DEPTH_STENCIL_OES
  , pattern GL_UNSIGNED_INT_24_8_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_packed_depth_stencil.txt GL_OES_packed_depth_stencil> extension is available.

gl_OES_packed_depth_stencil :: Bool
gl_OES_packed_depth_stencil = member "GL_OES_packed_depth_stencil" extensions
{-# NOINLINE gl_OES_packed_depth_stencil #-}