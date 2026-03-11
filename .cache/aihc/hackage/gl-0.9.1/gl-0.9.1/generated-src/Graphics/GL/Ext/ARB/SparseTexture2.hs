-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SparseTexture2 (
  -- * Extension Support
    gl_ARB_sparse_texture2
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sparse_texture2.txt GL_ARB_sparse_texture2> extension is available.

gl_ARB_sparse_texture2 :: Bool
gl_ARB_sparse_texture2 = member "GL_ARB_sparse_texture2" extensions
{-# NOINLINE gl_ARB_sparse_texture2 #-}