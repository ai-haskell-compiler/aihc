-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Mapbuffer (
  -- * Extension Support
    gl_OES_mapbuffer

  -- * GL_OES_mapbuffer
  , glGetBufferPointervOES
  , glMapBufferOES
  , glUnmapBufferOES
  , pattern GL_BUFFER_ACCESS_OES
  , pattern GL_BUFFER_MAPPED_OES
  , pattern GL_BUFFER_MAP_POINTER_OES
  , pattern GL_WRITE_ONLY_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_mapbuffer.txt GL_OES_mapbuffer> extension is available.

gl_OES_mapbuffer :: Bool
gl_OES_mapbuffer = member "GL_OES_mapbuffer" extensions
{-# NOINLINE gl_OES_mapbuffer #-}

-- | Usage: @'glGetBufferPointervOES' target pname params@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetBufferPointerv'.


glGetBufferPointervOES :: MonadIO m => GLenum -> GLenum -> Ptr (Ptr ()) -> m ()
glGetBufferPointervOES = ffienumenumPtrPtrVIOV glGetBufferPointervOESFunPtr

glGetBufferPointervOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetBufferPointervOESFunPtr = unsafePerformIO (getProcAddress "glGetBufferPointervOES")

{-# NOINLINE glGetBufferPointervOESFunPtr #-}

-- | Usage: @'glMapBufferOES' target access@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMapBuffer'.


glMapBufferOES :: MonadIO m => GLenum -> GLenum -> m (Ptr ())
glMapBufferOES = ffienumenumIOPtrV glMapBufferOESFunPtr

glMapBufferOESFunPtr :: FunPtr (GLenum -> GLenum -> IO (Ptr ()))
glMapBufferOESFunPtr = unsafePerformIO (getProcAddress "glMapBufferOES")

{-# NOINLINE glMapBufferOESFunPtr #-}

-- | Usage: @'glUnmapBufferOES' target@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUnmapBuffer'.


glUnmapBufferOES :: MonadIO m => GLenum -> m GLboolean
glUnmapBufferOES = ffienumIOboolean glUnmapBufferOESFunPtr

glUnmapBufferOESFunPtr :: FunPtr (GLenum -> IO GLboolean)
glUnmapBufferOESFunPtr = unsafePerformIO (getProcAddress "glUnmapBufferOES")

{-# NOINLINE glUnmapBufferOESFunPtr #-}

pattern GL_BUFFER_ACCESS_OES  :: (Eq a, Num a) => a

pattern GL_BUFFER_ACCESS_OES = 0x88BB

pattern GL_BUFFER_MAPPED_OES  :: (Eq a, Num a) => a

pattern GL_BUFFER_MAPPED_OES = 0x88BC

pattern GL_BUFFER_MAP_POINTER_OES  :: (Eq a, Num a) => a

pattern GL_BUFFER_MAP_POINTER_OES = 0x88BD

pattern GL_WRITE_ONLY_OES  :: (Eq a, Num a) => a

pattern GL_WRITE_ONLY_OES = 0x88B9