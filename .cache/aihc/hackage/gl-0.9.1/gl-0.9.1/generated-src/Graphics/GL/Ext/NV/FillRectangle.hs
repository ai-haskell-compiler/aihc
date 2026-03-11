-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FillRectangle (
  -- * Extension Support
    gl_NV_fill_rectangle

  -- * GL_NV_fill_rectangle
  , pattern GL_FILL_RECTANGLE_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/fill_rectangle.txt GL_NV_fill_rectangle> extension is available.

gl_NV_fill_rectangle :: Bool
gl_NV_fill_rectangle = member "GL_NV_fill_rectangle" extensions
{-# NOINLINE gl_NV_fill_rectangle #-}

pattern GL_FILL_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_FILL_RECTANGLE_NV = 0x933C