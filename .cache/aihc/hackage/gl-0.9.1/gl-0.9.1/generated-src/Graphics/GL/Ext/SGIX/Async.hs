-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Async (
  -- * Extension Support
    gl_SGIX_async

  -- * GL_SGIX_async
  , glAsyncMarkerSGIX
  , glDeleteAsyncMarkersSGIX
  , glFinishAsyncSGIX
  , glGenAsyncMarkersSGIX
  , glIsAsyncMarkerSGIX
  , glPollAsyncSGIX
  , pattern GL_ASYNC_MARKER_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/async.txt GL_SGIX_async> extension is available.

gl_SGIX_async :: Bool
gl_SGIX_async = member "GL_SGIX_async" extensions
{-# NOINLINE gl_SGIX_async #-}

-- | Usage: @'glAsyncMarkerSGIX' marker@


glAsyncMarkerSGIX :: MonadIO m => GLuint -> m ()
glAsyncMarkerSGIX = ffiuintIOV glAsyncMarkerSGIXFunPtr

glAsyncMarkerSGIXFunPtr :: FunPtr (GLuint -> IO ())
glAsyncMarkerSGIXFunPtr = unsafePerformIO (getProcAddress "glAsyncMarkerSGIX")

{-# NOINLINE glAsyncMarkerSGIXFunPtr #-}

-- | Usage: @'glDeleteAsyncMarkersSGIX' marker range@


glDeleteAsyncMarkersSGIX :: MonadIO m => GLuint -> GLsizei -> m ()
glDeleteAsyncMarkersSGIX = ffiuintsizeiIOV glDeleteAsyncMarkersSGIXFunPtr

glDeleteAsyncMarkersSGIXFunPtr :: FunPtr (GLuint -> GLsizei -> IO ())
glDeleteAsyncMarkersSGIXFunPtr = unsafePerformIO (getProcAddress "glDeleteAsyncMarkersSGIX")

{-# NOINLINE glDeleteAsyncMarkersSGIXFunPtr #-}

-- | Usage: @'glFinishAsyncSGIX' markerp@
--
-- The length of @markerp@ should be @1@.


glFinishAsyncSGIX :: MonadIO m => Ptr GLuint -> m GLint
glFinishAsyncSGIX = ffiPtruintIOint glFinishAsyncSGIXFunPtr

glFinishAsyncSGIXFunPtr :: FunPtr (Ptr GLuint -> IO GLint)
glFinishAsyncSGIXFunPtr = unsafePerformIO (getProcAddress "glFinishAsyncSGIX")

{-# NOINLINE glFinishAsyncSGIXFunPtr #-}

-- | Usage: @'glGenAsyncMarkersSGIX' range@


glGenAsyncMarkersSGIX :: MonadIO m => GLsizei -> m GLuint
glGenAsyncMarkersSGIX = ffisizeiIOuint glGenAsyncMarkersSGIXFunPtr

glGenAsyncMarkersSGIXFunPtr :: FunPtr (GLsizei -> IO GLuint)
glGenAsyncMarkersSGIXFunPtr = unsafePerformIO (getProcAddress "glGenAsyncMarkersSGIX")

{-# NOINLINE glGenAsyncMarkersSGIXFunPtr #-}

-- | Usage: @'glIsAsyncMarkerSGIX' marker@


glIsAsyncMarkerSGIX :: MonadIO m => GLuint -> m GLboolean
glIsAsyncMarkerSGIX = ffiuintIOboolean glIsAsyncMarkerSGIXFunPtr

glIsAsyncMarkerSGIXFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsAsyncMarkerSGIXFunPtr = unsafePerformIO (getProcAddress "glIsAsyncMarkerSGIX")

{-# NOINLINE glIsAsyncMarkerSGIXFunPtr #-}

-- | Usage: @'glPollAsyncSGIX' markerp@
--
-- The length of @markerp@ should be @1@.


glPollAsyncSGIX :: MonadIO m => Ptr GLuint -> m GLint
glPollAsyncSGIX = ffiPtruintIOint glPollAsyncSGIXFunPtr

glPollAsyncSGIXFunPtr :: FunPtr (Ptr GLuint -> IO GLint)
glPollAsyncSGIXFunPtr = unsafePerformIO (getProcAddress "glPollAsyncSGIX")

{-# NOINLINE glPollAsyncSGIXFunPtr #-}

pattern GL_ASYNC_MARKER_SGIX  :: (Eq a, Num a) => a

pattern GL_ASYNC_MARKER_SGIX = 0x8329