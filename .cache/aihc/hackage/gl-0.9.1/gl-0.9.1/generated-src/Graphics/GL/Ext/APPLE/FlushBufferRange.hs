-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.FlushBufferRange (
  -- * Extension Support
    gl_APPLE_flush_buffer_range

  -- * GL_APPLE_flush_buffer_range
  , glBufferParameteriAPPLE
  , glFlushMappedBufferRangeAPPLE
  , pattern GL_BUFFER_FLUSHING_UNMAP_APPLE
  , pattern GL_BUFFER_SERIALIZED_MODIFY_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/flush_buffer_range.txt GL_APPLE_flush_buffer_range> extension is available.

gl_APPLE_flush_buffer_range :: Bool
gl_APPLE_flush_buffer_range = member "GL_APPLE_flush_buffer_range" extensions
{-# NOINLINE gl_APPLE_flush_buffer_range #-}

-- | Usage: @'glBufferParameteriAPPLE' target pname param@


glBufferParameteriAPPLE :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glBufferParameteriAPPLE = ffienumenumintIOV glBufferParameteriAPPLEFunPtr

glBufferParameteriAPPLEFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glBufferParameteriAPPLEFunPtr = unsafePerformIO (getProcAddress "glBufferParameteriAPPLE")

{-# NOINLINE glBufferParameteriAPPLEFunPtr #-}

-- | Usage: @'glFlushMappedBufferRangeAPPLE' target offset size@
--
-- The parameter @offset@ is a @BufferOffset@.
--
-- The parameter @size@ is a @BufferSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFlushMappedBufferRange'.


glFlushMappedBufferRangeAPPLE :: MonadIO m => GLenum -> GLintptr -> GLsizeiptr -> m ()
glFlushMappedBufferRangeAPPLE = ffienumintptrsizeiptrIOV glFlushMappedBufferRangeAPPLEFunPtr

glFlushMappedBufferRangeAPPLEFunPtr :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
glFlushMappedBufferRangeAPPLEFunPtr = unsafePerformIO (getProcAddress "glFlushMappedBufferRangeAPPLE")

{-# NOINLINE glFlushMappedBufferRangeAPPLEFunPtr #-}

pattern GL_BUFFER_FLUSHING_UNMAP_APPLE  :: (Eq a, Num a) => a

pattern GL_BUFFER_FLUSHING_UNMAP_APPLE = 0x8A13

pattern GL_BUFFER_SERIALIZED_MODIFY_APPLE  :: (Eq a, Num a) => a

pattern GL_BUFFER_SERIALIZED_MODIFY_APPLE = 0x8A12