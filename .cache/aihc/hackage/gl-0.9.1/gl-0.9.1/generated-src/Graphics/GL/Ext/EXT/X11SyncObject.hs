-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.X11SyncObject (
  -- * Extension Support
    gl_EXT_x11_sync_object

  -- * GL_EXT_x11_sync_object
  , glImportSyncEXT
  , pattern GL_SYNC_X11_FENCE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/x11_sync_object.txt GL_EXT_x11_sync_object> extension is available.

gl_EXT_x11_sync_object :: Bool
gl_EXT_x11_sync_object = member "GL_EXT_x11_sync_object" extensions
{-# NOINLINE gl_EXT_x11_sync_object #-}

-- | Usage: @'glImportSyncEXT' external_sync_type external_sync flags@


glImportSyncEXT :: MonadIO m => GLenum -> GLintptr -> GLbitfield -> m GLsync
glImportSyncEXT = ffienumintptrbitfieldIOsync glImportSyncEXTFunPtr

glImportSyncEXTFunPtr :: FunPtr (GLenum -> GLintptr -> GLbitfield -> IO GLsync)
glImportSyncEXTFunPtr = unsafePerformIO (getProcAddress "glImportSyncEXT")

{-# NOINLINE glImportSyncEXTFunPtr #-}

pattern GL_SYNC_X11_FENCE_EXT  :: (Eq a, Num a) => a

pattern GL_SYNC_X11_FENCE_EXT = 0x90E1