-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VideoCapture (
  -- * Extension Support
    gl_NV_video_capture

  -- * GL_NV_video_capture
  , glBeginVideoCaptureNV
  , glBindVideoCaptureStreamBufferNV
  , glBindVideoCaptureStreamTextureNV
  , glEndVideoCaptureNV
  , glGetVideoCaptureStreamdvNV
  , glGetVideoCaptureStreamfvNV
  , glGetVideoCaptureStreamivNV
  , glGetVideoCaptureivNV
  , glVideoCaptureNV
  , glVideoCaptureStreamParameterdvNV
  , glVideoCaptureStreamParameterfvNV
  , glVideoCaptureStreamParameterivNV
  , pattern GL_FAILURE_NV
  , pattern GL_FIELD_LOWER_NV
  , pattern GL_FIELD_UPPER_NV
  , pattern GL_LAST_VIDEO_CAPTURE_STATUS_NV
  , pattern GL_NEXT_VIDEO_CAPTURE_BUFFER_STATUS_NV
  , pattern GL_NUM_VIDEO_CAPTURE_STREAMS_NV
  , pattern GL_PARTIAL_SUCCESS_NV
  , pattern GL_SUCCESS_NV
  , pattern GL_VIDEO_BUFFER_BINDING_NV
  , pattern GL_VIDEO_BUFFER_INTERNAL_FORMAT_NV
  , pattern GL_VIDEO_BUFFER_NV
  , pattern GL_VIDEO_BUFFER_PITCH_NV
  , pattern GL_VIDEO_CAPTURE_FIELD_LOWER_HEIGHT_NV
  , pattern GL_VIDEO_CAPTURE_FIELD_UPPER_HEIGHT_NV
  , pattern GL_VIDEO_CAPTURE_FRAME_HEIGHT_NV
  , pattern GL_VIDEO_CAPTURE_FRAME_WIDTH_NV
  , pattern GL_VIDEO_CAPTURE_SURFACE_ORIGIN_NV
  , pattern GL_VIDEO_CAPTURE_TO_422_SUPPORTED_NV
  , pattern GL_VIDEO_COLOR_CONVERSION_MATRIX_NV
  , pattern GL_VIDEO_COLOR_CONVERSION_MAX_NV
  , pattern GL_VIDEO_COLOR_CONVERSION_MIN_NV
  , pattern GL_VIDEO_COLOR_CONVERSION_OFFSET_NV
  , pattern GL_YCBAYCR8A_4224_NV
  , pattern GL_YCBYCR8_422_NV
  , pattern GL_Z4Y12Z4CB12Z4A12Z4Y12Z4CR12Z4A12_4224_NV
  , pattern GL_Z4Y12Z4CB12Z4CR12_444_NV
  , pattern GL_Z4Y12Z4CB12Z4Y12Z4CR12_422_NV
  , pattern GL_Z6Y10Z6CB10Z6A10Z6Y10Z6CR10Z6A10_4224_NV
  , pattern GL_Z6Y10Z6CB10Z6Y10Z6CR10_422_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/video_capture.txt GL_NV_video_capture> extension is available.

gl_NV_video_capture :: Bool
gl_NV_video_capture = member "GL_NV_video_capture" extensions
{-# NOINLINE gl_NV_video_capture #-}

-- | Usage: @'glBeginVideoCaptureNV' video_capture_slot@


glBeginVideoCaptureNV :: MonadIO m => GLuint -> m ()
glBeginVideoCaptureNV = ffiuintIOV glBeginVideoCaptureNVFunPtr

glBeginVideoCaptureNVFunPtr :: FunPtr (GLuint -> IO ())
glBeginVideoCaptureNVFunPtr = unsafePerformIO (getProcAddress "glBeginVideoCaptureNV")

{-# NOINLINE glBeginVideoCaptureNVFunPtr #-}

-- | Usage: @'glBindVideoCaptureStreamBufferNV' video_capture_slot stream frame_region offset@
--
-- The parameter @offset@ is a @BufferOffsetARB@.


glBindVideoCaptureStreamBufferNV :: MonadIO m => GLuint -> GLuint -> GLenum -> GLintptrARB -> m ()
glBindVideoCaptureStreamBufferNV = ffiuintuintenumintptrARBIOV glBindVideoCaptureStreamBufferNVFunPtr

glBindVideoCaptureStreamBufferNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptrARB -> IO ())
glBindVideoCaptureStreamBufferNVFunPtr = unsafePerformIO (getProcAddress "glBindVideoCaptureStreamBufferNV")

{-# NOINLINE glBindVideoCaptureStreamBufferNVFunPtr #-}

-- | Usage: @'glBindVideoCaptureStreamTextureNV' video_capture_slot stream frame_region target texture@


glBindVideoCaptureStreamTextureNV :: MonadIO m => GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> m ()
glBindVideoCaptureStreamTextureNV = ffiuintuintenumenumuintIOV glBindVideoCaptureStreamTextureNVFunPtr

glBindVideoCaptureStreamTextureNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> IO ())
glBindVideoCaptureStreamTextureNVFunPtr = unsafePerformIO (getProcAddress "glBindVideoCaptureStreamTextureNV")

{-# NOINLINE glBindVideoCaptureStreamTextureNVFunPtr #-}

-- | Usage: @'glEndVideoCaptureNV' video_capture_slot@


glEndVideoCaptureNV :: MonadIO m => GLuint -> m ()
glEndVideoCaptureNV = ffiuintIOV glEndVideoCaptureNVFunPtr

glEndVideoCaptureNVFunPtr :: FunPtr (GLuint -> IO ())
glEndVideoCaptureNVFunPtr = unsafePerformIO (getProcAddress "glEndVideoCaptureNV")

{-# NOINLINE glEndVideoCaptureNVFunPtr #-}

-- | Usage: @'glGetVideoCaptureStreamdvNV' video_capture_slot stream pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideoCaptureStreamdvNV :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLdouble -> m ()
glGetVideoCaptureStreamdvNV = ffiuintuintenumPtrdoubleIOV glGetVideoCaptureStreamdvNVFunPtr

glGetVideoCaptureStreamdvNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
glGetVideoCaptureStreamdvNVFunPtr = unsafePerformIO (getProcAddress "glGetVideoCaptureStreamdvNV")

{-# NOINLINE glGetVideoCaptureStreamdvNVFunPtr #-}

-- | Usage: @'glGetVideoCaptureStreamfvNV' video_capture_slot stream pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideoCaptureStreamfvNV :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetVideoCaptureStreamfvNV = ffiuintuintenumPtrfloatIOV glGetVideoCaptureStreamfvNVFunPtr

glGetVideoCaptureStreamfvNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetVideoCaptureStreamfvNVFunPtr = unsafePerformIO (getProcAddress "glGetVideoCaptureStreamfvNV")

{-# NOINLINE glGetVideoCaptureStreamfvNVFunPtr #-}

-- | Usage: @'glGetVideoCaptureStreamivNV' video_capture_slot stream pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideoCaptureStreamivNV :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLint -> m ()
glGetVideoCaptureStreamivNV = ffiuintuintenumPtrintIOV glGetVideoCaptureStreamivNVFunPtr

glGetVideoCaptureStreamivNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVideoCaptureStreamivNVFunPtr = unsafePerformIO (getProcAddress "glGetVideoCaptureStreamivNV")

{-# NOINLINE glGetVideoCaptureStreamivNVFunPtr #-}

-- | Usage: @'glGetVideoCaptureivNV' video_capture_slot pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideoCaptureivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVideoCaptureivNV = ffiuintenumPtrintIOV glGetVideoCaptureivNVFunPtr

glGetVideoCaptureivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVideoCaptureivNVFunPtr = unsafePerformIO (getProcAddress "glGetVideoCaptureivNV")

{-# NOINLINE glGetVideoCaptureivNVFunPtr #-}

-- | Usage: @'glVideoCaptureNV' video_capture_slot sequence_num capture_time@


glVideoCaptureNV :: MonadIO m => GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> m GLenum
glVideoCaptureNV = ffiuintPtruintPtruint64EXTIOenum glVideoCaptureNVFunPtr

glVideoCaptureNVFunPtr :: FunPtr (GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> IO GLenum)
glVideoCaptureNVFunPtr = unsafePerformIO (getProcAddress "glVideoCaptureNV")

{-# NOINLINE glVideoCaptureNVFunPtr #-}

-- | Usage: @'glVideoCaptureStreamParameterdvNV' video_capture_slot stream pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glVideoCaptureStreamParameterdvNV :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLdouble -> m ()
glVideoCaptureStreamParameterdvNV = ffiuintuintenumPtrdoubleIOV glVideoCaptureStreamParameterdvNVFunPtr

glVideoCaptureStreamParameterdvNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
glVideoCaptureStreamParameterdvNVFunPtr = unsafePerformIO (getProcAddress "glVideoCaptureStreamParameterdvNV")

{-# NOINLINE glVideoCaptureStreamParameterdvNVFunPtr #-}

-- | Usage: @'glVideoCaptureStreamParameterfvNV' video_capture_slot stream pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glVideoCaptureStreamParameterfvNV :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLfloat -> m ()
glVideoCaptureStreamParameterfvNV = ffiuintuintenumPtrfloatIOV glVideoCaptureStreamParameterfvNVFunPtr

glVideoCaptureStreamParameterfvNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
glVideoCaptureStreamParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glVideoCaptureStreamParameterfvNV")

{-# NOINLINE glVideoCaptureStreamParameterfvNVFunPtr #-}

-- | Usage: @'glVideoCaptureStreamParameterivNV' video_capture_slot stream pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glVideoCaptureStreamParameterivNV :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLint -> m ()
glVideoCaptureStreamParameterivNV = ffiuintuintenumPtrintIOV glVideoCaptureStreamParameterivNVFunPtr

glVideoCaptureStreamParameterivNVFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
glVideoCaptureStreamParameterivNVFunPtr = unsafePerformIO (getProcAddress "glVideoCaptureStreamParameterivNV")

{-# NOINLINE glVideoCaptureStreamParameterivNVFunPtr #-}

pattern GL_FAILURE_NV  :: (Eq a, Num a) => a

pattern GL_FAILURE_NV = 0x9030

pattern GL_FIELD_LOWER_NV  :: (Eq a, Num a) => a

pattern GL_FIELD_LOWER_NV = 0x9023

pattern GL_FIELD_UPPER_NV  :: (Eq a, Num a) => a

pattern GL_FIELD_UPPER_NV = 0x9022

pattern GL_LAST_VIDEO_CAPTURE_STATUS_NV  :: (Eq a, Num a) => a

pattern GL_LAST_VIDEO_CAPTURE_STATUS_NV = 0x9027

pattern GL_NEXT_VIDEO_CAPTURE_BUFFER_STATUS_NV  :: (Eq a, Num a) => a

pattern GL_NEXT_VIDEO_CAPTURE_BUFFER_STATUS_NV = 0x9025

pattern GL_NUM_VIDEO_CAPTURE_STREAMS_NV  :: (Eq a, Num a) => a

pattern GL_NUM_VIDEO_CAPTURE_STREAMS_NV = 0x9024

pattern GL_PARTIAL_SUCCESS_NV  :: (Eq a, Num a) => a

pattern GL_PARTIAL_SUCCESS_NV = 0x902E

pattern GL_SUCCESS_NV  :: (Eq a, Num a) => a

pattern GL_SUCCESS_NV = 0x902F

pattern GL_VIDEO_BUFFER_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_BUFFER_BINDING_NV = 0x9021

pattern GL_VIDEO_BUFFER_INTERNAL_FORMAT_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_BUFFER_INTERNAL_FORMAT_NV = 0x902D

pattern GL_VIDEO_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_BUFFER_NV = 0x9020

pattern GL_VIDEO_BUFFER_PITCH_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_BUFFER_PITCH_NV = 0x9028

pattern GL_VIDEO_CAPTURE_FIELD_LOWER_HEIGHT_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_CAPTURE_FIELD_LOWER_HEIGHT_NV = 0x903B

pattern GL_VIDEO_CAPTURE_FIELD_UPPER_HEIGHT_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_CAPTURE_FIELD_UPPER_HEIGHT_NV = 0x903A

pattern GL_VIDEO_CAPTURE_FRAME_HEIGHT_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_CAPTURE_FRAME_HEIGHT_NV = 0x9039

pattern GL_VIDEO_CAPTURE_FRAME_WIDTH_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_CAPTURE_FRAME_WIDTH_NV = 0x9038

pattern GL_VIDEO_CAPTURE_SURFACE_ORIGIN_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_CAPTURE_SURFACE_ORIGIN_NV = 0x903C

pattern GL_VIDEO_CAPTURE_TO_422_SUPPORTED_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_CAPTURE_TO_422_SUPPORTED_NV = 0x9026

pattern GL_VIDEO_COLOR_CONVERSION_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_COLOR_CONVERSION_MATRIX_NV = 0x9029

pattern GL_VIDEO_COLOR_CONVERSION_MAX_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_COLOR_CONVERSION_MAX_NV = 0x902A

pattern GL_VIDEO_COLOR_CONVERSION_MIN_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_COLOR_CONVERSION_MIN_NV = 0x902B

pattern GL_VIDEO_COLOR_CONVERSION_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_VIDEO_COLOR_CONVERSION_OFFSET_NV = 0x902C

pattern GL_YCBAYCR8A_4224_NV  :: (Eq a, Num a) => a

pattern GL_YCBAYCR8A_4224_NV = 0x9032

pattern GL_YCBYCR8_422_NV  :: (Eq a, Num a) => a

pattern GL_YCBYCR8_422_NV = 0x9031

pattern GL_Z4Y12Z4CB12Z4A12Z4Y12Z4CR12Z4A12_4224_NV  :: (Eq a, Num a) => a

pattern GL_Z4Y12Z4CB12Z4A12Z4Y12Z4CR12Z4A12_4224_NV = 0x9036

pattern GL_Z4Y12Z4CB12Z4CR12_444_NV  :: (Eq a, Num a) => a

pattern GL_Z4Y12Z4CB12Z4CR12_444_NV = 0x9037

pattern GL_Z4Y12Z4CB12Z4Y12Z4CR12_422_NV  :: (Eq a, Num a) => a

pattern GL_Z4Y12Z4CB12Z4Y12Z4CR12_422_NV = 0x9035

pattern GL_Z6Y10Z6CB10Z6A10Z6Y10Z6CR10Z6A10_4224_NV  :: (Eq a, Num a) => a

pattern GL_Z6Y10Z6CB10Z6A10Z6Y10Z6CR10Z6A10_4224_NV = 0x9034

pattern GL_Z6Y10Z6CB10Z6Y10Z6CR10_422_NV  :: (Eq a, Num a) => a

pattern GL_Z6Y10Z6CB10Z6Y10Z6CR10_422_NV = 0x9033