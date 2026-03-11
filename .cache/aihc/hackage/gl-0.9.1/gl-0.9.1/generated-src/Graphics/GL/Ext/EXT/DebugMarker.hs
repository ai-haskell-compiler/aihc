-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DebugMarker (
  -- * Extension Support
    gl_EXT_debug_marker

  -- * GL_EXT_debug_marker
  , glInsertEventMarkerEXT
  , glPopGroupMarkerEXT
  , glPushGroupMarkerEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_debug_marker.txt GL_EXT_debug_marker> extension is available.

gl_EXT_debug_marker :: Bool
gl_EXT_debug_marker = member "GL_EXT_debug_marker" extensions
{-# NOINLINE gl_EXT_debug_marker #-}

-- | Usage: @'glInsertEventMarkerEXT' length marker@


glInsertEventMarkerEXT :: MonadIO m => GLsizei -> Ptr GLchar -> m ()
glInsertEventMarkerEXT = ffisizeiPtrcharIOV glInsertEventMarkerEXTFunPtr

glInsertEventMarkerEXTFunPtr :: FunPtr (GLsizei -> Ptr GLchar -> IO ())
glInsertEventMarkerEXTFunPtr = unsafePerformIO (getProcAddress "glInsertEventMarkerEXT")

{-# NOINLINE glInsertEventMarkerEXTFunPtr #-}

-- | Usage: @'glPopGroupMarkerEXT'@


glPopGroupMarkerEXT :: MonadIO m => m ()
glPopGroupMarkerEXT = ffiIOV glPopGroupMarkerEXTFunPtr

glPopGroupMarkerEXTFunPtr :: FunPtr (IO ())
glPopGroupMarkerEXTFunPtr = unsafePerformIO (getProcAddress "glPopGroupMarkerEXT")

{-# NOINLINE glPopGroupMarkerEXTFunPtr #-}

-- | Usage: @'glPushGroupMarkerEXT' length marker@


glPushGroupMarkerEXT :: MonadIO m => GLsizei -> Ptr GLchar -> m ()
glPushGroupMarkerEXT = ffisizeiPtrcharIOV glPushGroupMarkerEXTFunPtr

glPushGroupMarkerEXTFunPtr :: FunPtr (GLsizei -> Ptr GLchar -> IO ())
glPushGroupMarkerEXTFunPtr = unsafePerformIO (getProcAddress "glPushGroupMarkerEXT")

{-# NOINLINE glPushGroupMarkerEXTFunPtr #-}