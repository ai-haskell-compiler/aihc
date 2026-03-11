-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ClEvent (
  -- * Extension Support
    gl_ARB_cl_event

  -- * GL_ARB_cl_event
  , glCreateSyncFromCLeventARB
  , pattern GL_SYNC_CL_EVENT_ARB
  , pattern GL_SYNC_CL_EVENT_COMPLETE_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/cl_event.txt GL_ARB_cl_event> extension is available.

gl_ARB_cl_event :: Bool
gl_ARB_cl_event = member "GL_ARB_cl_event" extensions
{-# NOINLINE gl_ARB_cl_event #-}

-- | Usage: @'glCreateSyncFromCLeventARB' context event flags@
--
-- The parameter @context@ is a @cl_context@.
--
-- The parameter @event@ is a @cl_event@.


glCreateSyncFromCLeventARB :: MonadIO m => Ptr () -> Ptr () -> GLbitfield -> m GLsync
glCreateSyncFromCLeventARB = ffiPtrVPtrVbitfieldIOsync glCreateSyncFromCLeventARBFunPtr

glCreateSyncFromCLeventARBFunPtr :: FunPtr (Ptr () -> Ptr () -> GLbitfield -> IO GLsync)
glCreateSyncFromCLeventARBFunPtr = unsafePerformIO (getProcAddress "glCreateSyncFromCLeventARB")

{-# NOINLINE glCreateSyncFromCLeventARBFunPtr #-}

pattern GL_SYNC_CL_EVENT_ARB  :: (Eq a, Num a) => a

pattern GL_SYNC_CL_EVENT_ARB = 0x8240

pattern GL_SYNC_CL_EVENT_COMPLETE_ARB  :: (Eq a, Num a) => a

pattern GL_SYNC_CL_EVENT_COMPLETE_ARB = 0x8241