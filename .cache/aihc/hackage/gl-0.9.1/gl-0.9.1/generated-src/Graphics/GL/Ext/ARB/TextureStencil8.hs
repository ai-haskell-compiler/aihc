-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureStencil8 (
  -- * Extension Support
    gl_ARB_texture_stencil8

  -- * GL_ARB_texture_stencil8
  , pattern GL_STENCIL_INDEX
  , pattern GL_STENCIL_INDEX8
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_stencil8.txt GL_ARB_texture_stencil8> extension is available.

gl_ARB_texture_stencil8 :: Bool
gl_ARB_texture_stencil8 = member "GL_ARB_texture_stencil8" extensions
{-# NOINLINE gl_ARB_texture_stencil8 #-}