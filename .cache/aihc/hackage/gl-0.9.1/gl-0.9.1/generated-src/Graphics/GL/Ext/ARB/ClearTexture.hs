-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ClearTexture (
  -- * Extension Support
    gl_ARB_clear_texture

  -- * GL_ARB_clear_texture
  , glClearTexImage
  , glClearTexSubImage
  , pattern GL_CLEAR_TEXTURE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/clear_texture.txt GL_ARB_clear_texture> extension is available.

gl_ARB_clear_texture :: Bool
gl_ARB_clear_texture = member "GL_ARB_clear_texture" extensions
{-# NOINLINE gl_ARB_clear_texture #-}