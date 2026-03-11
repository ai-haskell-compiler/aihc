-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureBarrier (
  -- * Extension Support
    gl_ARB_texture_barrier

  -- * GL_ARB_texture_barrier
  , glTextureBarrier
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_barrier.txt GL_ARB_texture_barrier> extension is available.

gl_ARB_texture_barrier :: Bool
gl_ARB_texture_barrier = member "GL_ARB_texture_barrier" extensions
{-# NOINLINE gl_ARB_texture_barrier #-}