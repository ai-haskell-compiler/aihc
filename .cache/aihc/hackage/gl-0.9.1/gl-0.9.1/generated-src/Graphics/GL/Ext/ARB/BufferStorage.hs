-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.BufferStorage (
  -- * Extension Support
    gl_ARB_buffer_storage

  -- * GL_ARB_buffer_storage
  , glBufferStorage
  , pattern GL_BUFFER_IMMUTABLE_STORAGE
  , pattern GL_BUFFER_STORAGE_FLAGS
  , pattern GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT
  , pattern GL_CLIENT_STORAGE_BIT
  , pattern GL_DYNAMIC_STORAGE_BIT
  , pattern GL_MAP_COHERENT_BIT
  , pattern GL_MAP_PERSISTENT_BIT
  , pattern GL_MAP_READ_BIT
  , pattern GL_MAP_WRITE_BIT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/buffer_storage.txt GL_ARB_buffer_storage> extension is available.

gl_ARB_buffer_storage :: Bool
gl_ARB_buffer_storage = member "GL_ARB_buffer_storage" extensions
{-# NOINLINE gl_ARB_buffer_storage #-}