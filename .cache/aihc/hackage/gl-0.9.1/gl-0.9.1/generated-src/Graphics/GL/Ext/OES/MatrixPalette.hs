-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.MatrixPalette (
  -- * Extension Support
    gl_OES_matrix_palette

  -- * GL_OES_matrix_palette
  , glCurrentPaletteMatrixOES
  , glLoadPaletteFromModelViewMatrixOES
  , glMatrixIndexPointerOES
  , glWeightPointerOES
  , pattern GL_CURRENT_PALETTE_MATRIX_OES
  , pattern GL_MATRIX_INDEX_ARRAY_BUFFER_BINDING_OES
  , pattern GL_MATRIX_INDEX_ARRAY_OES
  , pattern GL_MATRIX_INDEX_ARRAY_POINTER_OES
  , pattern GL_MATRIX_INDEX_ARRAY_SIZE_OES
  , pattern GL_MATRIX_INDEX_ARRAY_STRIDE_OES
  , pattern GL_MATRIX_INDEX_ARRAY_TYPE_OES
  , pattern GL_MATRIX_PALETTE_OES
  , pattern GL_MAX_PALETTE_MATRICES_OES
  , pattern GL_MAX_VERTEX_UNITS_OES
  , pattern GL_WEIGHT_ARRAY_BUFFER_BINDING_OES
  , pattern GL_WEIGHT_ARRAY_OES
  , pattern GL_WEIGHT_ARRAY_POINTER_OES
  , pattern GL_WEIGHT_ARRAY_SIZE_OES
  , pattern GL_WEIGHT_ARRAY_STRIDE_OES
  , pattern GL_WEIGHT_ARRAY_TYPE_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_matrix_palette.txt GL_OES_matrix_palette> extension is available.

gl_OES_matrix_palette :: Bool
gl_OES_matrix_palette = member "GL_OES_matrix_palette" extensions
{-# NOINLINE gl_OES_matrix_palette #-}

-- | Usage: @'glCurrentPaletteMatrixOES' matrixpaletteindex@


glCurrentPaletteMatrixOES :: MonadIO m => GLuint -> m ()
glCurrentPaletteMatrixOES = ffiuintIOV glCurrentPaletteMatrixOESFunPtr

glCurrentPaletteMatrixOESFunPtr :: FunPtr (GLuint -> IO ())
glCurrentPaletteMatrixOESFunPtr = unsafePerformIO (getProcAddress "glCurrentPaletteMatrixOES")

{-# NOINLINE glCurrentPaletteMatrixOESFunPtr #-}

-- | Usage: @'glLoadPaletteFromModelViewMatrixOES'@


glLoadPaletteFromModelViewMatrixOES :: MonadIO m => m ()
glLoadPaletteFromModelViewMatrixOES = ffiIOV glLoadPaletteFromModelViewMatrixOESFunPtr

glLoadPaletteFromModelViewMatrixOESFunPtr :: FunPtr (IO ())
glLoadPaletteFromModelViewMatrixOESFunPtr = unsafePerformIO (getProcAddress "glLoadPaletteFromModelViewMatrixOES")

{-# NOINLINE glLoadPaletteFromModelViewMatrixOESFunPtr #-}

-- | Usage: @'glMatrixIndexPointerOES' size type stride pointer@
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.


glMatrixIndexPointerOES :: MonadIO m => GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glMatrixIndexPointerOES = ffiintenumsizeiPtrVIOV glMatrixIndexPointerOESFunPtr

glMatrixIndexPointerOESFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glMatrixIndexPointerOESFunPtr = unsafePerformIO (getProcAddress "glMatrixIndexPointerOES")

{-# NOINLINE glMatrixIndexPointerOESFunPtr #-}

-- | Usage: @'glWeightPointerOES' size type stride pointer@
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glWeightPointerOES :: MonadIO m => GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glWeightPointerOES = ffiintenumsizeiPtrVIOV glWeightPointerOESFunPtr

glWeightPointerOESFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glWeightPointerOESFunPtr = unsafePerformIO (getProcAddress "glWeightPointerOES")

{-# NOINLINE glWeightPointerOESFunPtr #-}

pattern GL_CURRENT_PALETTE_MATRIX_OES  :: (Eq a, Num a) => a

pattern GL_CURRENT_PALETTE_MATRIX_OES = 0x8843

pattern GL_MATRIX_INDEX_ARRAY_BUFFER_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_BUFFER_BINDING_OES = 0x8B9E

pattern GL_MATRIX_INDEX_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_OES = 0x8844

pattern GL_MATRIX_INDEX_ARRAY_POINTER_OES  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_POINTER_OES = 0x8849

pattern GL_MATRIX_INDEX_ARRAY_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_SIZE_OES = 0x8846

pattern GL_MATRIX_INDEX_ARRAY_STRIDE_OES  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_STRIDE_OES = 0x8848

pattern GL_MATRIX_INDEX_ARRAY_TYPE_OES  :: (Eq a, Num a) => a

pattern GL_MATRIX_INDEX_ARRAY_TYPE_OES = 0x8847

pattern GL_MATRIX_PALETTE_OES  :: (Eq a, Num a) => a

pattern GL_MATRIX_PALETTE_OES = 0x8840

pattern GL_MAX_PALETTE_MATRICES_OES  :: (Eq a, Num a) => a

pattern GL_MAX_PALETTE_MATRICES_OES = 0x8842

pattern GL_MAX_VERTEX_UNITS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_UNITS_OES = 0x86A4

pattern GL_WEIGHT_ARRAY_BUFFER_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_BUFFER_BINDING_OES = 0x889E

pattern GL_WEIGHT_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_OES = 0x86AD

pattern GL_WEIGHT_ARRAY_POINTER_OES  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_POINTER_OES = 0x86AC

pattern GL_WEIGHT_ARRAY_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_SIZE_OES = 0x86AB

pattern GL_WEIGHT_ARRAY_STRIDE_OES  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_STRIDE_OES = 0x86AA

pattern GL_WEIGHT_ARRAY_TYPE_OES  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_TYPE_OES = 0x86A9