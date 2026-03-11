-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.GeometryShader4 (
  -- * Extension Support
    gl_NV_geometry_shader4
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/geometry_shader4.txt GL_NV_geometry_shader4> extension is available.

gl_NV_geometry_shader4 :: Bool
gl_NV_geometry_shader4 = member "GL_NV_geometry_shader4" extensions
{-# NOINLINE gl_NV_geometry_shader4 #-}