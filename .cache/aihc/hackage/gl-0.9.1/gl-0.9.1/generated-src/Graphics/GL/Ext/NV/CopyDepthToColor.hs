-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.CopyDepthToColor (
  -- * Extension Support
    gl_NV_copy_depth_to_color

  -- * GL_NV_copy_depth_to_color
  , pattern GL_DEPTH_STENCIL_TO_BGRA_NV
  , pattern GL_DEPTH_STENCIL_TO_RGBA_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/copy_depth_to_color.txt GL_NV_copy_depth_to_color> extension is available.

gl_NV_copy_depth_to_color :: Bool
gl_NV_copy_depth_to_color = member "GL_NV_copy_depth_to_color" extensions
{-# NOINLINE gl_NV_copy_depth_to_color #-}

pattern GL_DEPTH_STENCIL_TO_BGRA_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_STENCIL_TO_BGRA_NV = 0x886F

pattern GL_DEPTH_STENCIL_TO_RGBA_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_STENCIL_TO_RGBA_NV = 0x886E