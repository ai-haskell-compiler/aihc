-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.ClientStorage (
  -- * Extension Support
    gl_APPLE_client_storage

  -- * GL_APPLE_client_storage
  , pattern GL_UNPACK_CLIENT_STORAGE_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/client_storage.txt GL_APPLE_client_storage> extension is available.

gl_APPLE_client_storage :: Bool
gl_APPLE_client_storage = member "GL_APPLE_client_storage" extensions
{-# NOINLINE gl_APPLE_client_storage #-}

pattern GL_UNPACK_CLIENT_STORAGE_APPLE  :: (Eq a, Num a) => a

pattern GL_UNPACK_CLIENT_STORAGE_APPLE = 0x85B2