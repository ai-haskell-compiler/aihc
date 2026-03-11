-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.DeepTexture3D (
  -- * Extension Support
    gl_NV_deep_texture3D

  -- * GL_NV_deep_texture3D
  , pattern GL_MAX_DEEP_3D_TEXTURE_DEPTH_NV
  , pattern GL_MAX_DEEP_3D_TEXTURE_WIDTH_HEIGHT_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/deep_texture3D.txt GL_NV_deep_texture3D> extension is available.

gl_NV_deep_texture3D :: Bool
gl_NV_deep_texture3D = member "GL_NV_deep_texture3D" extensions
{-# NOINLINE gl_NV_deep_texture3D #-}

pattern GL_MAX_DEEP_3D_TEXTURE_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_MAX_DEEP_3D_TEXTURE_DEPTH_NV = 0x90D1

pattern GL_MAX_DEEP_3D_TEXTURE_WIDTH_HEIGHT_NV  :: (Eq a, Num a) => a

pattern GL_MAX_DEEP_3D_TEXTURE_WIDTH_HEIGHT_NV = 0x90D0