-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PixelDataRange (
  -- * Extension Support
    gl_NV_pixel_data_range

  -- * GL_NV_pixel_data_range
  , glFlushPixelDataRangeNV
  , glPixelDataRangeNV
  , pattern GL_READ_PIXEL_DATA_RANGE_LENGTH_NV
  , pattern GL_READ_PIXEL_DATA_RANGE_NV
  , pattern GL_READ_PIXEL_DATA_RANGE_POINTER_NV
  , pattern GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV
  , pattern GL_WRITE_PIXEL_DATA_RANGE_NV
  , pattern GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/pixel_data_range.txt GL_NV_pixel_data_range> extension is available.

gl_NV_pixel_data_range :: Bool
gl_NV_pixel_data_range = member "GL_NV_pixel_data_range" extensions
{-# NOINLINE gl_NV_pixel_data_range #-}

-- | Usage: @'glFlushPixelDataRangeNV' target@
--
-- The parameter @target@ is a @PixelDataRangeTargetNV@.


glFlushPixelDataRangeNV :: MonadIO m => GLenum -> m ()
glFlushPixelDataRangeNV = ffienumIOV glFlushPixelDataRangeNVFunPtr

glFlushPixelDataRangeNVFunPtr :: FunPtr (GLenum -> IO ())
glFlushPixelDataRangeNVFunPtr = unsafePerformIO (getProcAddress "glFlushPixelDataRangeNV")

{-# NOINLINE glFlushPixelDataRangeNVFunPtr #-}

-- | Usage: @'glPixelDataRangeNV' target length pointer@
--
-- The parameter @target@ is a @PixelDataRangeTargetNV@.
--
-- The length of @pointer@ should be @length@.


glPixelDataRangeNV :: MonadIO m => GLenum -> GLsizei -> Ptr () -> m ()
glPixelDataRangeNV = ffienumsizeiPtrVIOV glPixelDataRangeNVFunPtr

glPixelDataRangeNVFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr () -> IO ())
glPixelDataRangeNVFunPtr = unsafePerformIO (getProcAddress "glPixelDataRangeNV")

{-# NOINLINE glPixelDataRangeNVFunPtr #-}

pattern GL_READ_PIXEL_DATA_RANGE_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_READ_PIXEL_DATA_RANGE_LENGTH_NV = 0x887B

pattern GL_READ_PIXEL_DATA_RANGE_NV  :: (Eq a, Num a) => a

pattern GL_READ_PIXEL_DATA_RANGE_NV = 0x8879

pattern GL_READ_PIXEL_DATA_RANGE_POINTER_NV  :: (Eq a, Num a) => a

pattern GL_READ_PIXEL_DATA_RANGE_POINTER_NV = 0x887D

pattern GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV = 0x887A

pattern GL_WRITE_PIXEL_DATA_RANGE_NV  :: (Eq a, Num a) => a

pattern GL_WRITE_PIXEL_DATA_RANGE_NV = 0x8878

pattern GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV  :: (Eq a, Num a) => a

pattern GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV = 0x887C