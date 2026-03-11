-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureView (
  -- * Extension Support
    gl_ARB_texture_view

  -- * GL_ARB_texture_view
  , glTextureView
  , pattern GL_TEXTURE_IMMUTABLE_LEVELS
  , pattern GL_TEXTURE_VIEW_MIN_LAYER
  , pattern GL_TEXTURE_VIEW_MIN_LEVEL
  , pattern GL_TEXTURE_VIEW_NUM_LAYERS
  , pattern GL_TEXTURE_VIEW_NUM_LEVELS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_view.txt GL_ARB_texture_view> extension is available.

gl_ARB_texture_view :: Bool
gl_ARB_texture_view = member "GL_ARB_texture_view" extensions
{-# NOINLINE gl_ARB_texture_view #-}