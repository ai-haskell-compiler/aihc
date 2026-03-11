-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INTEL.ParallelArrays (
  -- * Extension Support
    gl_INTEL_parallel_arrays

  -- * GL_INTEL_parallel_arrays
  , glColorPointervINTEL
  , glNormalPointervINTEL
  , glTexCoordPointervINTEL
  , glVertexPointervINTEL
  , pattern GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL
  , pattern GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL
  , pattern GL_PARALLEL_ARRAYS_INTEL
  , pattern GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL
  , pattern GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/INTEL/parallel_arrays.txt GL_INTEL_parallel_arrays> extension is available.

gl_INTEL_parallel_arrays :: Bool
gl_INTEL_parallel_arrays = member "GL_INTEL_parallel_arrays" extensions
{-# NOINLINE gl_INTEL_parallel_arrays #-}

-- | Usage: @'glColorPointervINTEL' size type pointer@
--
-- The parameter @type@ is a @VertexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @4@.


glColorPointervINTEL :: MonadIO m => GLint -> GLenum -> Ptr (Ptr ()) -> m ()
glColorPointervINTEL = ffiintenumPtrPtrVIOV glColorPointervINTELFunPtr

glColorPointervINTELFunPtr :: FunPtr (GLint -> GLenum -> Ptr (Ptr ()) -> IO ())
glColorPointervINTELFunPtr = unsafePerformIO (getProcAddress "glColorPointervINTEL")

{-# NOINLINE glColorPointervINTELFunPtr #-}

-- | Usage: @'glNormalPointervINTEL' type pointer@
--
-- The parameter @type@ is a @NormalPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @4@.


glNormalPointervINTEL :: MonadIO m => GLenum -> Ptr (Ptr ()) -> m ()
glNormalPointervINTEL = ffienumPtrPtrVIOV glNormalPointervINTELFunPtr

glNormalPointervINTELFunPtr :: FunPtr (GLenum -> Ptr (Ptr ()) -> IO ())
glNormalPointervINTELFunPtr = unsafePerformIO (getProcAddress "glNormalPointervINTEL")

{-# NOINLINE glNormalPointervINTELFunPtr #-}

-- | Usage: @'glTexCoordPointervINTEL' size type pointer@
--
-- The parameter @type@ is a @VertexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @4@.


glTexCoordPointervINTEL :: MonadIO m => GLint -> GLenum -> Ptr (Ptr ()) -> m ()
glTexCoordPointervINTEL = ffiintenumPtrPtrVIOV glTexCoordPointervINTELFunPtr

glTexCoordPointervINTELFunPtr :: FunPtr (GLint -> GLenum -> Ptr (Ptr ()) -> IO ())
glTexCoordPointervINTELFunPtr = unsafePerformIO (getProcAddress "glTexCoordPointervINTEL")

{-# NOINLINE glTexCoordPointervINTELFunPtr #-}

-- | Usage: @'glVertexPointervINTEL' size type pointer@
--
-- The parameter @type@ is a @VertexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @4@.


glVertexPointervINTEL :: MonadIO m => GLint -> GLenum -> Ptr (Ptr ()) -> m ()
glVertexPointervINTEL = ffiintenumPtrPtrVIOV glVertexPointervINTELFunPtr

glVertexPointervINTELFunPtr :: FunPtr (GLint -> GLenum -> Ptr (Ptr ()) -> IO ())
glVertexPointervINTELFunPtr = unsafePerformIO (getProcAddress "glVertexPointervINTEL")

{-# NOINLINE glVertexPointervINTELFunPtr #-}

pattern GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL = 0x83F7

pattern GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL = 0x83F6

pattern GL_PARALLEL_ARRAYS_INTEL  :: (Eq a, Num a) => a

pattern GL_PARALLEL_ARRAYS_INTEL = 0x83F4

pattern GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL = 0x83F8

pattern GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL = 0x83F5