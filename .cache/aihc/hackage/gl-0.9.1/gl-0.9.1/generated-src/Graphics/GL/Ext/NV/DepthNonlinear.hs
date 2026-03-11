-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.DepthNonlinear (
  -- * Extension Support
    gl_NV_depth_nonlinear

  -- * GL_NV_depth_nonlinear
  , pattern GL_DEPTH_COMPONENT16_NONLINEAR_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_depth_nonlinear.txt GL_NV_depth_nonlinear> extension is available.

gl_NV_depth_nonlinear :: Bool
gl_NV_depth_nonlinear = member "GL_NV_depth_nonlinear" extensions
{-# NOINLINE gl_NV_depth_nonlinear #-}

pattern GL_DEPTH_COMPONENT16_NONLINEAR_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT16_NONLINEAR_NV = 0x8E2C