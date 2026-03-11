-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SparseTextureClamp (
  -- * Extension Support
    gl_ARB_sparse_texture_clamp
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sparse_texture_clamp.txt GL_ARB_sparse_texture_clamp> extension is available.

gl_ARB_sparse_texture_clamp :: Bool
gl_ARB_sparse_texture_clamp = member "GL_ARB_sparse_texture_clamp" extensions
{-# NOINLINE gl_ARB_sparse_texture_clamp #-}