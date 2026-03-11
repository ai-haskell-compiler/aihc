-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.DepthClamp (
  -- * Extension Support
    gl_NV_depth_clamp

  -- * GL_NV_depth_clamp
  , pattern GL_DEPTH_CLAMP_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/depth_clamp.txt GL_NV_depth_clamp> extension is available.

gl_NV_depth_clamp :: Bool
gl_NV_depth_clamp = member "GL_NV_depth_clamp" extensions
{-# NOINLINE gl_NV_depth_clamp #-}

pattern GL_DEPTH_CLAMP_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_CLAMP_NV = 0x864F