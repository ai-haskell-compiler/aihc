-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.MatrixPalette (
  -- * Extension Support
    gl_ARB_matrix_palette

  -- * GL_ARB_matrix_palette
  , glCurrentPaletteMatrixARB
  , glMatrixIndexPointerARB
  , glMatrixIndexubvARB
  , glMatrixIndexuivARB
  , glMatrixIndexusvARB
  , pattern GL_CURRENT_MATRIX_INDEX_ARB
  , pattern GL_CURRENT_PALETTE_MATRIX_ARB
  , pattern GL_MATRIX_INDEX_ARRAY_ARB
  , pattern GL_MATRIX_INDEX_ARRAY_POINTER_ARB
  , pattern GL_MATRIX_INDEX_ARRAY_SIZE_ARB
  , pattern GL_MATRIX_INDEX_ARRAY_STRIDE_ARB
  , pattern GL_MATRIX_INDEX_ARRAY_TYPE_ARB
  , pattern GL_MATRIX_PALETTE_ARB
  , pattern GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB
  , pattern GL_MAX_PALETTE_MATRICES_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/matrix_palette.txt GL_ARB_matrix_palette> extension is available.

gl_ARB_matrix_palette :: Bool
gl_ARB_matrix_palette = member "GL_ARB_matrix_palette" extensions
{-# NOINLINE gl_ARB_matrix_palette #-}

-- | Usage: @'glCurrentPaletteMatrixARB' index@


glCurrentPaletteMatrixARB :: MonadIO m => GLint -> m ()
glCurrentPaletteMatrixARB = ffiintIOV glCurrentPaletteMatrixARBFunPtr

glCurrentPaletteMatrixARBFunPtr :: FunPtr (GLint -> IO ())
glCurrentPaletteMatrixARBFunPtr = unsafePerformIO (getProcAddress "glCurrentPaletteMatrixARB")

{-# NOINLINE glCurrentPaletteMatrixARBFunPtr #-}

-- | Usage: @'glMatrixIndexPointerARB' size type stride pointer@
--
-- The parameter @type@ is a @MatrixIndexPointerTypeARB@.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.


glMatrixIndexPointerARB :: MonadIO m => GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glMatrixIndexPointerARB = ffiintenumsizeiPtrVIOV glMatrixIndexPointerARBFunPtr

glMatrixIndexPointerARBFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glMatrixIndexPointerARBFunPtr = unsafePerformIO (getProcAddress "glMatrixIndexPointerARB")

{-# NOINLINE glMatrixIndexPointerARBFunPtr #-}

-- | Usage: @'glMatrixIndexubvARB' size indices@
--
-- The length of @indices@ should be @size@.


glMatrixIndexubvARB :: MonadIO m => GLint -> Ptr GLubyte -> m ()
glMatrixIndexubvARB = ffiintPtrubyteIOV glMatrixIndexubvARBFunPtr

glMatrixIndexubvARBFunPtr :: FunPtr (GLint -> Ptr GLubyte -> IO ())
glMatrixIndexubvARBFunPtr = unsafePerformIO (getProcAddress "glMatrixIndexubvARB")

{-# NOINLINE glMatrixIndexubvARBFunPtr #-}

-- | Usage: @'glMatrixIndexuivARB' size indices@
--
-- The length of @indices@ should be @size@.


glMatrixIndexuivARB :: MonadIO m => GLint -> Ptr GLuint -> m ()
glMatrixIndexuivARB = ffiintPtruintIOV glMatrixIndexuivARBFunPtr

glMatrixIndexuivARBFunPtr :: FunPtr (GLint -> Ptr GLuint -> IO ())
glMatrixIndexuivARBFunPtr = unsafePerformIO (getProcAddress "glMatrixIndexuivARB")

{-# NOINLINE glMatrixIndexuivARBFunPtr #-}

-- | Usage: @'glMatrixIndexusvARB' size indices@
--
-- The length of @indices@ should be @size@.


glMatrixIndexusvARB :: MonadIO m => GLint -> Ptr GLushort -> m ()
glMatrixIndexusvARB = ffiintPtrushortIOV glMatrixIndexusvARBFunPtr

glMatrixIndexusvARBFunPtr :: FunPtr (GLint -> Ptr GLushort -> IO ())
glMatrixIndexusvARBFunPtr = unsafePerformIO (getProcAddress "glMatrixIndexusvARB")

{-# NOINLINE glMatrixIndexusvARBFunPtr #-}

pattern GL_CURRENT_MATRIX_INDEX_ARB  :: (Eq a, Num a) => a

pattern GL_CURRENT_MATRIX_INDEX_ARB = 0x8845

pattern GL_CURRENT_PALETTE_MATRIX_ARB  :: (Eq a, Num a) => a

pattern GL_CURRENT_PALETTE_MATRIX_ARB = 0x8843

pattern GL_MATRIX_INDEX_ARRAY_ARB  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_ARB = 0x8844

pattern GL_MATRIX_INDEX_ARRAY_POINTER_ARB  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_POINTER_ARB = 0x8849

pattern GL_MATRIX_INDEX_ARRAY_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_SIZE_ARB = 0x8846

pattern GL_MATRIX_INDEX_ARRAY_STRIDE_ARB  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_STRIDE_ARB = 0x8848

pattern GL_MATRIX_INDEX_ARRAY_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_TYPE_ARB = 0x8847

pattern GL_MATRIX_PALETTE_ARB  :: (Eq a, Num a) => a

pattern GL_MATRIX_PALETTE_ARB = 0x8840

pattern GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB = 0x8841

pattern GL_MAX_PALETTE_MATRICES_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_PALETTE_MATRICES_ARB = 0x8842