-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.VertexArrayRange (
  -- * Extension Support
    gl_APPLE_vertex_array_range

  -- * GL_APPLE_vertex_array_range
  , glFlushVertexArrayRangeAPPLE
  , glVertexArrayParameteriAPPLE
  , glVertexArrayRangeAPPLE
  , pattern GL_STORAGE_CACHED_APPLE
  , pattern GL_STORAGE_CLIENT_APPLE
  , pattern GL_STORAGE_SHARED_APPLE
  , pattern GL_VERTEX_ARRAY_RANGE_APPLE
  , pattern GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE
  , pattern GL_VERTEX_ARRAY_RANGE_POINTER_APPLE
  , pattern GL_VERTEX_ARRAY_STORAGE_HINT_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/vertex_array_range.txt GL_APPLE_vertex_array_range> extension is available.

gl_APPLE_vertex_array_range :: Bool
gl_APPLE_vertex_array_range = member "GL_APPLE_vertex_array_range" extensions
{-# NOINLINE gl_APPLE_vertex_array_range #-}

-- | Usage: @'glFlushVertexArrayRangeAPPLE' length pointer@
--
-- The length of @pointer@ should be @length@.


glFlushVertexArrayRangeAPPLE :: MonadIO m => GLsizei -> Ptr () -> m ()
glFlushVertexArrayRangeAPPLE = ffisizeiPtrVIOV glFlushVertexArrayRangeAPPLEFunPtr

glFlushVertexArrayRangeAPPLEFunPtr :: FunPtr (GLsizei -> Ptr () -> IO ())
glFlushVertexArrayRangeAPPLEFunPtr = unsafePerformIO (getProcAddress "glFlushVertexArrayRangeAPPLE")

{-# NOINLINE glFlushVertexArrayRangeAPPLEFunPtr #-}

-- | Usage: @'glVertexArrayParameteriAPPLE' pname param@
--
-- The parameter @pname@ is a @VertexArrayPNameAPPLE@.


glVertexArrayParameteriAPPLE :: MonadIO m => GLenum -> GLint -> m ()
glVertexArrayParameteriAPPLE = ffienumintIOV glVertexArrayParameteriAPPLEFunPtr

glVertexArrayParameteriAPPLEFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glVertexArrayParameteriAPPLEFunPtr = unsafePerformIO (getProcAddress "glVertexArrayParameteriAPPLE")

{-# NOINLINE glVertexArrayParameteriAPPLEFunPtr #-}

-- | Usage: @'glVertexArrayRangeAPPLE' length pointer@
--
-- The length of @pointer@ should be @length@.


glVertexArrayRangeAPPLE :: MonadIO m => GLsizei -> Ptr () -> m ()
glVertexArrayRangeAPPLE = ffisizeiPtrVIOV glVertexArrayRangeAPPLEFunPtr

glVertexArrayRangeAPPLEFunPtr :: FunPtr (GLsizei -> Ptr () -> IO ())
glVertexArrayRangeAPPLEFunPtr = unsafePerformIO (getProcAddress "glVertexArrayRangeAPPLE")

{-# NOINLINE glVertexArrayRangeAPPLEFunPtr #-}

pattern GL_STORAGE_CLIENT_APPLE  :: (Eq a, Num a) => a

pattern GL_STORAGE_CLIENT_APPLE = 0x85B4

pattern GL_VERTEX_ARRAY_RANGE_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_APPLE = 0x851D

pattern GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE = 0x851E

pattern GL_VERTEX_ARRAY_RANGE_POINTER_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_POINTER_APPLE = 0x8521

pattern GL_VERTEX_ARRAY_STORAGE_HINT_APPLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_STORAGE_HINT_APPLE = 0x851F