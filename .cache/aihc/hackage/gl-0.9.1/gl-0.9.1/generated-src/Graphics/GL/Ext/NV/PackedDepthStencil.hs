-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PackedDepthStencil (
  -- * Extension Support
    gl_NV_packed_depth_stencil

  -- * GL_NV_packed_depth_stencil
  , pattern GL_DEPTH_STENCIL_NV
  , pattern GL_UNSIGNED_INT_24_8_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/packed_depth_stencil.txt GL_NV_packed_depth_stencil> extension is available.

gl_NV_packed_depth_stencil :: Bool
gl_NV_packed_depth_stencil = member "GL_NV_packed_depth_stencil" extensions
{-# NOINLINE gl_NV_packed_depth_stencil #-}

pattern GL_DEPTH_STENCIL_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_STENCIL_NV = 0x84F9

pattern GL_UNSIGNED_INT_24_8_NV  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_24_8_NV = 0x84FA