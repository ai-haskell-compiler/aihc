-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.PointSizeArray (
  -- * Extension Support
    gl_OES_point_size_array

  -- * GL_OES_point_size_array
  , glPointSizePointerOES
  , pattern GL_POINT_SIZE_ARRAY_BUFFER_BINDING_OES
  , pattern GL_POINT_SIZE_ARRAY_OES
  , pattern GL_POINT_SIZE_ARRAY_POINTER_OES
  , pattern GL_POINT_SIZE_ARRAY_STRIDE_OES
  , pattern GL_POINT_SIZE_ARRAY_TYPE_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_point_size_array.txt GL_OES_point_size_array> extension is available.

gl_OES_point_size_array :: Bool
gl_OES_point_size_array = member "GL_OES_point_size_array" extensions
{-# NOINLINE gl_OES_point_size_array #-}

-- | Usage: @'glPointSizePointerOES' type stride pointer@
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glPointSizePointerOES :: MonadIO m => GLenum -> GLsizei -> Ptr () -> m ()
glPointSizePointerOES = ffienumsizeiPtrVIOV glPointSizePointerOESFunPtr

glPointSizePointerOESFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr () -> IO ())
glPointSizePointerOESFunPtr = unsafePerformIO (getProcAddress "glPointSizePointerOES")

{-# NOINLINE glPointSizePointerOESFunPtr #-}

pattern GL_POINT_SIZE_ARRAY_BUFFER_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_ARRAY_BUFFER_BINDING_OES = 0x8B9F

pattern GL_POINT_SIZE_ARRAY_OES  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_ARRAY_OES = 0x8B9C

pattern GL_POINT_SIZE_ARRAY_POINTER_OES  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_ARRAY_POINTER_OES = 0x898C

pattern GL_POINT_SIZE_ARRAY_STRIDE_OES  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_ARRAY_STRIDE_OES = 0x898B

pattern GL_POINT_SIZE_ARRAY_TYPE_OES  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_ARRAY_TYPE_OES = 0x898A