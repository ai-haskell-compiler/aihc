-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureStorageMultisample (
  -- * Extension Support
    gl_ARB_texture_storage_multisample

  -- * GL_ARB_texture_storage_multisample
  , glTexStorage2DMultisample
  , glTexStorage3DMultisample
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_storage_multisample.txt GL_ARB_texture_storage_multisample> extension is available.

gl_ARB_texture_storage_multisample :: Bool
gl_ARB_texture_storage_multisample = member "GL_ARB_texture_storage_multisample" extensions
{-# NOINLINE gl_ARB_texture_storage_multisample #-}