-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.GeometryPointSize (
  -- * Extension Support
    gl_EXT_geometry_point_size
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_geometry_point_size.txt GL_EXT_geometry_point_size> extension is available.

gl_EXT_geometry_point_size :: Bool
gl_EXT_geometry_point_size = member "GL_EXT_geometry_point_size" extensions
{-# NOINLINE gl_EXT_geometry_point_size #-}