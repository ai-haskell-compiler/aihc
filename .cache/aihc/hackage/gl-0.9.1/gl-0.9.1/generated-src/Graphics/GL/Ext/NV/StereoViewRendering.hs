-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.StereoViewRendering (
  -- * Extension Support
    gl_NV_stereo_view_rendering
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/stereo_view_rendering.txt GL_NV_stereo_view_rendering> extension is available.

gl_NV_stereo_view_rendering :: Bool
gl_NV_stereo_view_rendering = member "GL_NV_stereo_view_rendering" extensions
{-# NOINLINE gl_NV_stereo_view_rendering #-}