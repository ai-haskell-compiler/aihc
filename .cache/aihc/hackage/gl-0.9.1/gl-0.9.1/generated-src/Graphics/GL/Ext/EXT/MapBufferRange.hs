-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.MapBufferRange (
  -- * Extension Support
    gl_EXT_map_buffer_range

  -- * GL_EXT_map_buffer_range
  , glFlushMappedBufferRangeEXT
  , glMapBufferRangeEXT
  , pattern GL_MAP_FLUSH_EXPLICIT_BIT_EXT
  , pattern GL_MAP_INVALIDATE_BUFFER_BIT_EXT
  , pattern GL_MAP_INVALIDATE_RANGE_BIT_EXT
  , pattern GL_MAP_READ_BIT_EXT
  , pattern GL_MAP_UNSYNCHRONIZED_BIT_EXT
  , pattern GL_MAP_WRITE_BIT_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_map_buffer_range.txt GL_EXT_map_buffer_range> extension is available.

gl_EXT_map_buffer_range :: Bool
gl_EXT_map_buffer_range = member "GL_EXT_map_buffer_range" extensions
{-# NOINLINE gl_EXT_map_buffer_range #-}

-- | Usage: @'glFlushMappedBufferRangeEXT' target offset length@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFlushMappedBufferRange'.


glFlushMappedBufferRangeEXT :: MonadIO m => GLenum -> GLintptr -> GLsizeiptr -> m ()
glFlushMappedBufferRangeEXT = ffienumintptrsizeiptrIOV glFlushMappedBufferRangeEXTFunPtr

glFlushMappedBufferRangeEXTFunPtr :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
glFlushMappedBufferRangeEXTFunPtr = unsafePerformIO (getProcAddress "glFlushMappedBufferRangeEXT")

{-# NOINLINE glFlushMappedBufferRangeEXTFunPtr #-}

-- | Usage: @'glMapBufferRangeEXT' target offset length access@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMapBufferRange'.


glMapBufferRangeEXT :: MonadIO m => GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> m (Ptr ())
glMapBufferRangeEXT = ffienumintptrsizeiptrbitfieldIOPtrV glMapBufferRangeEXTFunPtr

glMapBufferRangeEXTFunPtr :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ()))
glMapBufferRangeEXTFunPtr = unsafePerformIO (getProcAddress "glMapBufferRangeEXT")

{-# NOINLINE glMapBufferRangeEXTFunPtr #-}

pattern GL_MAP_FLUSH_EXPLICIT_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_FLUSH_EXPLICIT_BIT_EXT = 0x0010

pattern GL_MAP_INVALIDATE_BUFFER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_INVALIDATE_BUFFER_BIT_EXT = 0x0008

pattern GL_MAP_INVALIDATE_RANGE_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_INVALIDATE_RANGE_BIT_EXT = 0x0004

pattern GL_MAP_READ_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_READ_BIT_EXT = 0x0001

pattern GL_MAP_UNSYNCHRONIZED_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_UNSYNCHRONIZED_BIT_EXT = 0x0020

pattern GL_MAP_WRITE_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP_WRITE_BIT_EXT = 0x0002