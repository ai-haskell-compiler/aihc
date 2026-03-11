-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.Sync (
  -- * Extension Support
    gl_ARB_sync

  -- * GL_ARB_sync
  , glClientWaitSync
  , glDeleteSync
  , glFenceSync
  , glGetInteger64v
  , glGetSynciv
  , glIsSync
  , glWaitSync
  , pattern GL_ALREADY_SIGNALED
  , pattern GL_CONDITION_SATISFIED
  , pattern GL_MAX_SERVER_WAIT_TIMEOUT
  , pattern GL_OBJECT_TYPE
  , pattern GL_SIGNALED
  , pattern GL_SYNC_CONDITION
  , pattern GL_SYNC_FENCE
  , pattern GL_SYNC_FLAGS
  , pattern GL_SYNC_FLUSH_COMMANDS_BIT
  , pattern GL_SYNC_GPU_COMMANDS_COMPLETE
  , pattern GL_SYNC_STATUS
  , pattern GL_TIMEOUT_EXPIRED
  , pattern GL_TIMEOUT_IGNORED
  , pattern GL_UNSIGNALED
  , pattern GL_WAIT_FAILED
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sync.txt GL_ARB_sync> extension is available.

gl_ARB_sync :: Bool
gl_ARB_sync = member "GL_ARB_sync" extensions
{-# NOINLINE gl_ARB_sync #-}