-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.Sync (
  -- * Extension Support
    gl_APPLE_sync

  -- * GL_APPLE_sync
  , glClientWaitSyncAPPLE
  , glDeleteSyncAPPLE
  , glFenceSyncAPPLE
  , glGetInteger64vAPPLE
  , glGetSyncivAPPLE
  , glIsSyncAPPLE
  , glWaitSyncAPPLE
  , pattern GL_ALREADY_SIGNALED_APPLE
  , pattern GL_CONDITION_SATISFIED_APPLE
  , pattern GL_MAX_SERVER_WAIT_TIMEOUT_APPLE
  , pattern GL_OBJECT_TYPE_APPLE
  , pattern GL_SIGNALED_APPLE
  , pattern GL_SYNC_CONDITION_APPLE
  , pattern GL_SYNC_FENCE_APPLE
  , pattern GL_SYNC_FLAGS_APPLE
  , pattern GL_SYNC_FLUSH_COMMANDS_BIT_APPLE
  , pattern GL_SYNC_GPU_COMMANDS_COMPLETE_APPLE
  , pattern GL_SYNC_OBJECT_APPLE
  , pattern GL_SYNC_STATUS_APPLE
  , pattern GL_TIMEOUT_EXPIRED_APPLE
  , pattern GL_TIMEOUT_IGNORED_APPLE
  , pattern GL_UNSIGNALED_APPLE
  , pattern GL_WAIT_FAILED_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_sync.txt GL_APPLE_sync> extension is available.

gl_APPLE_sync :: Bool
gl_APPLE_sync = member "GL_APPLE_sync" extensions
{-# NOINLINE gl_APPLE_sync #-}

-- | Usage: @'glClientWaitSyncAPPLE' sync flags timeout@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glClientWaitSync'.


glClientWaitSyncAPPLE :: MonadIO m => GLsync -> GLbitfield -> GLuint64 -> m GLenum
glClientWaitSyncAPPLE = ffisyncbitfielduint64IOenum glClientWaitSyncAPPLEFunPtr

glClientWaitSyncAPPLEFunPtr :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO GLenum)
glClientWaitSyncAPPLEFunPtr = unsafePerformIO (getProcAddress "glClientWaitSyncAPPLE")

{-# NOINLINE glClientWaitSyncAPPLEFunPtr #-}

-- | Usage: @'glDeleteSyncAPPLE' sync@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteSync'.


glDeleteSyncAPPLE :: MonadIO m => GLsync -> m ()
glDeleteSyncAPPLE = ffisyncIOV glDeleteSyncAPPLEFunPtr

glDeleteSyncAPPLEFunPtr :: FunPtr (GLsync -> IO ())
glDeleteSyncAPPLEFunPtr = unsafePerformIO (getProcAddress "glDeleteSyncAPPLE")

{-# NOINLINE glDeleteSyncAPPLEFunPtr #-}

-- | Usage: @'glFenceSyncAPPLE' condition flags@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFenceSync'.


glFenceSyncAPPLE :: MonadIO m => GLenum -> GLbitfield -> m GLsync
glFenceSyncAPPLE = ffienumbitfieldIOsync glFenceSyncAPPLEFunPtr

glFenceSyncAPPLEFunPtr :: FunPtr (GLenum -> GLbitfield -> IO GLsync)
glFenceSyncAPPLEFunPtr = unsafePerformIO (getProcAddress "glFenceSyncAPPLE")

{-# NOINLINE glFenceSyncAPPLEFunPtr #-}

-- | Usage: @'glGetInteger64vAPPLE' pname params@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetInteger64v'.


glGetInteger64vAPPLE :: MonadIO m => GLenum -> Ptr GLint64 -> m ()
glGetInteger64vAPPLE = ffienumPtrint64IOV glGetInteger64vAPPLEFunPtr

glGetInteger64vAPPLEFunPtr :: FunPtr (GLenum -> Ptr GLint64 -> IO ())
glGetInteger64vAPPLEFunPtr = unsafePerformIO (getProcAddress "glGetInteger64vAPPLE")

{-# NOINLINE glGetInteger64vAPPLEFunPtr #-}

-- | Usage: @'glGetSyncivAPPLE' sync pname bufSize length values@
--
-- The length of @values@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetSynciv'.


glGetSyncivAPPLE :: MonadIO m => GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> m ()
glGetSyncivAPPLE = ffisyncenumsizeiPtrsizeiPtrintIOV glGetSyncivAPPLEFunPtr

glGetSyncivAPPLEFunPtr :: FunPtr (GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
glGetSyncivAPPLEFunPtr = unsafePerformIO (getProcAddress "glGetSyncivAPPLE")

{-# NOINLINE glGetSyncivAPPLEFunPtr #-}

-- | Usage: @'glIsSyncAPPLE' sync@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsSync'.


glIsSyncAPPLE :: MonadIO m => GLsync -> m GLboolean
glIsSyncAPPLE = ffisyncIOboolean glIsSyncAPPLEFunPtr

glIsSyncAPPLEFunPtr :: FunPtr (GLsync -> IO GLboolean)
glIsSyncAPPLEFunPtr = unsafePerformIO (getProcAddress "glIsSyncAPPLE")

{-# NOINLINE glIsSyncAPPLEFunPtr #-}

-- | Usage: @'glWaitSyncAPPLE' sync flags timeout@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWaitSync'.


glWaitSyncAPPLE :: MonadIO m => GLsync -> GLbitfield -> GLuint64 -> m ()
glWaitSyncAPPLE = ffisyncbitfielduint64IOV glWaitSyncAPPLEFunPtr

glWaitSyncAPPLEFunPtr :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO ())
glWaitSyncAPPLEFunPtr = unsafePerformIO (getProcAddress "glWaitSyncAPPLE")

{-# NOINLINE glWaitSyncAPPLEFunPtr #-}

pattern GL_ALREADY_SIGNALED_APPLE  :: (Eq a, Num a) => a

pattern GL_ALREADY_SIGNALED_APPLE = 0x911A

pattern GL_CONDITION_SATISFIED_APPLE  :: (Eq a, Num a) => a

pattern GL_CONDITION_SATISFIED_APPLE = 0x911C

pattern GL_MAX_SERVER_WAIT_TIMEOUT_APPLE  :: (Eq a, Num a) => a

pattern GL_MAX_SERVER_WAIT_TIMEOUT_APPLE = 0x9111

pattern GL_OBJECT_TYPE_APPLE  :: (Eq a, Num a) => a

pattern GL_OBJECT_TYPE_APPLE = 0x9112

pattern GL_SIGNALED_APPLE  :: (Eq a, Num a) => a

pattern GL_SIGNALED_APPLE = 0x9119

pattern GL_SYNC_CONDITION_APPLE  :: (Eq a, Num a) => a

pattern GL_SYNC_CONDITION_APPLE = 0x9113

pattern GL_SYNC_FENCE_APPLE  :: (Eq a, Num a) => a

pattern GL_SYNC_FENCE_APPLE = 0x9116

pattern GL_SYNC_FLAGS_APPLE  :: (Eq a, Num a) => a

pattern GL_SYNC_FLAGS_APPLE = 0x9115

pattern GL_SYNC_FLUSH_COMMANDS_BIT_APPLE  :: (Eq a, Num a) => a

pattern GL_SYNC_FLUSH_COMMANDS_BIT_APPLE = 0x00000001

pattern GL_SYNC_GPU_COMMANDS_COMPLETE_APPLE  :: (Eq a, Num a) => a

pattern GL_SYNC_GPU_COMMANDS_COMPLETE_APPLE = 0x9117

pattern GL_SYNC_OBJECT_APPLE  :: (Eq a, Num a) => a

pattern GL_SYNC_OBJECT_APPLE = 0x8A53

pattern GL_SYNC_STATUS_APPLE  :: (Eq a, Num a) => a

pattern GL_SYNC_STATUS_APPLE = 0x9114

pattern GL_TIMEOUT_EXPIRED_APPLE  :: (Eq a, Num a) => a

pattern GL_TIMEOUT_EXPIRED_APPLE = 0x911B

pattern GL_TIMEOUT_IGNORED_APPLE  :: (Eq a, Num a) => a

pattern GL_TIMEOUT_IGNORED_APPLE = 0xFFFFFFFFFFFFFFFF

pattern GL_UNSIGNALED_APPLE  :: (Eq a, Num a) => a

pattern GL_UNSIGNALED_APPLE = 0x9118

pattern GL_WAIT_FAILED_APPLE  :: (Eq a, Num a) => a

pattern GL_WAIT_FAILED_APPLE = 0x911D