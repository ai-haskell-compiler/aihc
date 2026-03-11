-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.MapObjectBuffer (
  -- * Extension Support
    gl_ATI_map_object_buffer

  -- * GL_ATI_map_object_buffer
  , glMapObjectBufferATI
  , glUnmapObjectBufferATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/map_object_buffer.txt GL_ATI_map_object_buffer> extension is available.

gl_ATI_map_object_buffer :: Bool
gl_ATI_map_object_buffer = member "GL_ATI_map_object_buffer" extensions
{-# NOINLINE gl_ATI_map_object_buffer #-}

-- | Usage: @'glMapObjectBufferATI' buffer@


glMapObjectBufferATI :: MonadIO m => GLuint -> m (Ptr ())
glMapObjectBufferATI = ffiuintIOPtrV glMapObjectBufferATIFunPtr

glMapObjectBufferATIFunPtr :: FunPtr (GLuint -> IO (Ptr ()))
glMapObjectBufferATIFunPtr = unsafePerformIO (getProcAddress "glMapObjectBufferATI")

{-# NOINLINE glMapObjectBufferATIFunPtr #-}

-- | Usage: @'glUnmapObjectBufferATI' buffer@


glUnmapObjectBufferATI :: MonadIO m => GLuint -> m ()
glUnmapObjectBufferATI = ffiuintIOV glUnmapObjectBufferATIFunPtr

glUnmapObjectBufferATIFunPtr :: FunPtr (GLuint -> IO ())
glUnmapObjectBufferATIFunPtr = unsafePerformIO (getProcAddress "glUnmapObjectBufferATI")

{-# NOINLINE glUnmapObjectBufferATIFunPtr #-}