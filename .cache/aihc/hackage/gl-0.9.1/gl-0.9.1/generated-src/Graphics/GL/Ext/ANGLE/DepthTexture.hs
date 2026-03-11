-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.DepthTexture (
  -- * Extension Support
    gl_ANGLE_depth_texture

  -- * GL_ANGLE_depth_texture
  , pattern GL_DEPTH24_STENCIL8_OES
  , pattern GL_DEPTH_COMPONENT
  , pattern GL_DEPTH_COMPONENT16
  , pattern GL_DEPTH_COMPONENT32_OES
  , pattern GL_DEPTH_STENCIL_OES
  , pattern GL_UNSIGNED_INT
  , pattern GL_UNSIGNED_INT_24_8_OES
  , pattern GL_UNSIGNED_SHORT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_depth_texture.txt GL_ANGLE_depth_texture> extension is available.

gl_ANGLE_depth_texture :: Bool
gl_ANGLE_depth_texture = member "GL_ANGLE_depth_texture" extensions
{-# NOINLINE gl_ANGLE_depth_texture #-}