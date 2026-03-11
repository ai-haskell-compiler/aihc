-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureStorage (
  -- * Extension Support
    gl_ARB_texture_storage

  -- * GL_ARB_texture_storage
  , glTexStorage1D
  , glTexStorage2D
  , glTexStorage3D
  , pattern GL_TEXTURE_IMMUTABLE_FORMAT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_storage.txt GL_ARB_texture_storage> extension is available.

gl_ARB_texture_storage :: Bool
gl_ARB_texture_storage = member "GL_ARB_texture_storage" extensions
{-# NOINLINE gl_ARB_texture_storage #-}