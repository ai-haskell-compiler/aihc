-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.DepthTexture (
  -- * Extension Support
    gl_OES_depth_texture

  -- * GL_OES_depth_texture
  , pattern GL_DEPTH_COMPONENT
  , pattern GL_UNSIGNED_INT
  , pattern GL_UNSIGNED_SHORT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_depth_texture.txt GL_OES_depth_texture> extension is available.

gl_OES_depth_texture :: Bool
gl_OES_depth_texture = member "GL_OES_depth_texture" extensions
{-# NOINLINE gl_OES_depth_texture #-}