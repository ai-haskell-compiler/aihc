-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.IndexTexture (
  -- * Extension Support
    gl_EXT_index_texture
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/index_texture.txt GL_EXT_index_texture> extension is available.

gl_EXT_index_texture :: Bool
gl_EXT_index_texture = member "GL_EXT_index_texture" extensions
{-# NOINLINE gl_EXT_index_texture #-}