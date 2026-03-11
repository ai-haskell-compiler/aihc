-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexArrayRange (
  -- * Extension Support
    gl_NV_vertex_array_range

  -- * GL_NV_vertex_array_range
  , glFlushVertexArrayRangeNV
  , glVertexArrayRangeNV
  , pattern GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV
  , pattern GL_VERTEX_ARRAY_RANGE_LENGTH_NV
  , pattern GL_VERTEX_ARRAY_RANGE_NV
  , pattern GL_VERTEX_ARRAY_RANGE_POINTER_NV
  , pattern GL_VERTEX_ARRAY_RANGE_VALID_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_array_range.txt GL_NV_vertex_array_range> extension is available.

gl_NV_vertex_array_range :: Bool
gl_NV_vertex_array_range = member "GL_NV_vertex_array_range" extensions
{-# NOINLINE gl_NV_vertex_array_range #-}

-- | Usage: @'glFlushVertexArrayRangeNV'@


glFlushVertexArrayRangeNV :: MonadIO m => m ()
glFlushVertexArrayRangeNV = ffiIOV glFlushVertexArrayRangeNVFunPtr

glFlushVertexArrayRangeNVFunPtr :: FunPtr (IO ())
glFlushVertexArrayRangeNVFunPtr = unsafePerformIO (getProcAddress "glFlushVertexArrayRangeNV")

{-# NOINLINE glFlushVertexArrayRangeNVFunPtr #-}

-- | Usage: @'glVertexArrayRangeNV' length pointer@
--
-- The length of @pointer@ should be @COMPSIZE(length)@.


glVertexArrayRangeNV :: MonadIO m => GLsizei -> Ptr () -> m ()
glVertexArrayRangeNV = ffisizeiPtrVIOV glVertexArrayRangeNVFunPtr

glVertexArrayRangeNVFunPtr :: FunPtr (GLsizei -> Ptr () -> IO ())
glVertexArrayRangeNVFunPtr = unsafePerformIO (getProcAddress "glVertexArrayRangeNV")

{-# NOINLINE glVertexArrayRangeNVFunPtr #-}

pattern GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = 0x8520

pattern GL_VERTEX_ARRAY_RANGE_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_LENGTH_NV = 0x851E

pattern GL_VERTEX_ARRAY_RANGE_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_NV = 0x851D

pattern GL_VERTEX_ARRAY_RANGE_POINTER_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_POINTER_NV = 0x8521

pattern GL_VERTEX_ARRAY_RANGE_VALID_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_VALID_NV = 0x851F