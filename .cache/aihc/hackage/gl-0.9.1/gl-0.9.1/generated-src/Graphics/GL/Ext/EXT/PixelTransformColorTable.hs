-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PixelTransformColorTable (
  -- * Extension Support
    gl_EXT_pixel_transform_color_table
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/pixel_transform_color_table.txt GL_EXT_pixel_transform_color_table> extension is available.

gl_EXT_pixel_transform_color_table :: Bool
gl_EXT_pixel_transform_color_table = member "GL_EXT_pixel_transform_color_table" extensions
{-# NOINLINE gl_EXT_pixel_transform_color_table #-}