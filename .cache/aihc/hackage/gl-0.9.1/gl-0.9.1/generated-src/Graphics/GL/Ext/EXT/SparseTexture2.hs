-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SparseTexture2 (
  -- * Extension Support
    gl_EXT_sparse_texture2
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/sparse_texture2.txt GL_EXT_sparse_texture2> extension is available.

gl_EXT_sparse_texture2 :: Bool
gl_EXT_sparse_texture2 = member "GL_EXT_sparse_texture2" extensions
{-# NOINLINE gl_EXT_sparse_texture2 #-}