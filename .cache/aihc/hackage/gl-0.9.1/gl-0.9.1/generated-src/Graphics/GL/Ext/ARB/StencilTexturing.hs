-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.StencilTexturing (
  -- * Extension Support
    gl_ARB_stencil_texturing

  -- * GL_ARB_stencil_texturing
  , pattern GL_DEPTH_STENCIL_TEXTURE_MODE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/stencil_texturing.txt GL_ARB_stencil_texturing> extension is available.

gl_ARB_stencil_texturing :: Bool
gl_ARB_stencil_texturing = member "GL_ARB_stencil_texturing" extensions
{-# NOINLINE gl_ARB_stencil_texturing #-}