-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SparseBuffer (
  -- * Extension Support
    gl_ARB_sparse_buffer

  -- * GL_ARB_sparse_buffer
  , glBufferPageCommitmentARB
  , glNamedBufferPageCommitmentARB
  , glNamedBufferPageCommitmentEXT
  , pattern GL_SPARSE_BUFFER_PAGE_SIZE_ARB
  , pattern GL_SPARSE_STORAGE_BIT_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sparse_buffer.txt GL_ARB_sparse_buffer> extension is available.

gl_ARB_sparse_buffer :: Bool
gl_ARB_sparse_buffer = member "GL_ARB_sparse_buffer" extensions
{-# NOINLINE gl_ARB_sparse_buffer #-}

-- | Usage: @'glBufferPageCommitmentARB' target offset size commit@


glBufferPageCommitmentARB :: MonadIO m => GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> m ()
glBufferPageCommitmentARB = ffienumintptrsizeiptrbooleanIOV glBufferPageCommitmentARBFunPtr

glBufferPageCommitmentARBFunPtr :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
glBufferPageCommitmentARBFunPtr = unsafePerformIO (getProcAddress "glBufferPageCommitmentARB")

{-# NOINLINE glBufferPageCommitmentARBFunPtr #-}

-- | Usage: @'glNamedBufferPageCommitmentARB' buffer offset size commit@


glNamedBufferPageCommitmentARB :: MonadIO m => GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> m ()
glNamedBufferPageCommitmentARB = ffiuintintptrsizeiptrbooleanIOV glNamedBufferPageCommitmentARBFunPtr

glNamedBufferPageCommitmentARBFunPtr :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
glNamedBufferPageCommitmentARBFunPtr = unsafePerformIO (getProcAddress "glNamedBufferPageCommitmentARB")

{-# NOINLINE glNamedBufferPageCommitmentARBFunPtr #-}

-- | Usage: @'glNamedBufferPageCommitmentEXT' buffer offset size commit@


glNamedBufferPageCommitmentEXT :: MonadIO m => GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> m ()
glNamedBufferPageCommitmentEXT = ffiuintintptrsizeiptrbooleanIOV glNamedBufferPageCommitmentEXTFunPtr

glNamedBufferPageCommitmentEXTFunPtr :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
glNamedBufferPageCommitmentEXTFunPtr = unsafePerformIO (getProcAddress "glNamedBufferPageCommitmentEXT")

{-# NOINLINE glNamedBufferPageCommitmentEXTFunPtr #-}

pattern GL_SPARSE_BUFFER_PAGE_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_SPARSE_BUFFER_PAGE_SIZE_ARB = 0x82F8

pattern GL_SPARSE_STORAGE_BIT_ARB  :: (Eq a, Num a) => a

pattern GL_SPARSE_STORAGE_BIT_ARB = 0x0400