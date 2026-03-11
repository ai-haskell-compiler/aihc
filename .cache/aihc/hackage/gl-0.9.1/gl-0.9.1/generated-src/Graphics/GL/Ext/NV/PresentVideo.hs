-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PresentVideo (
  -- * Extension Support
    gl_NV_present_video

  -- * GL_NV_present_video
  , glGetVideoi64vNV
  , glGetVideoivNV
  , glGetVideoui64vNV
  , glGetVideouivNV
  , glPresentFrameDualFillNV
  , glPresentFrameKeyedNV
  , pattern GL_CURRENT_TIME_NV
  , pattern GL_FIELDS_NV
  , pattern GL_FRAME_NV
  , pattern GL_NUM_FILL_STREAMS_NV
  , pattern GL_PRESENT_DURATION_NV
  , pattern GL_PRESENT_TIME_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/present_video.txt GL_NV_present_video> extension is available.

gl_NV_present_video :: Bool
gl_NV_present_video = member "GL_NV_present_video" extensions
{-# NOINLINE gl_NV_present_video #-}

-- | Usage: @'glGetVideoi64vNV' video_slot pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideoi64vNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint64EXT -> m ()
glGetVideoi64vNV = ffiuintenumPtrint64EXTIOV glGetVideoi64vNVFunPtr

glGetVideoi64vNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ())
glGetVideoi64vNVFunPtr = unsafePerformIO (getProcAddress "glGetVideoi64vNV")

{-# NOINLINE glGetVideoi64vNVFunPtr #-}

-- | Usage: @'glGetVideoivNV' video_slot pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideoivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVideoivNV = ffiuintenumPtrintIOV glGetVideoivNVFunPtr

glGetVideoivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVideoivNVFunPtr = unsafePerformIO (getProcAddress "glGetVideoivNV")

{-# NOINLINE glGetVideoivNVFunPtr #-}

-- | Usage: @'glGetVideoui64vNV' video_slot pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideoui64vNV :: MonadIO m => GLuint -> GLenum -> Ptr GLuint64EXT -> m ()
glGetVideoui64vNV = ffiuintenumPtruint64EXTIOV glGetVideoui64vNVFunPtr

glGetVideoui64vNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
glGetVideoui64vNVFunPtr = unsafePerformIO (getProcAddress "glGetVideoui64vNV")

{-# NOINLINE glGetVideoui64vNVFunPtr #-}

-- | Usage: @'glGetVideouivNV' video_slot pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVideouivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glGetVideouivNV = ffiuintenumPtruintIOV glGetVideouivNVFunPtr

glGetVideouivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glGetVideouivNVFunPtr = unsafePerformIO (getProcAddress "glGetVideouivNV")

{-# NOINLINE glGetVideouivNVFunPtr #-}

-- | Usage: @'glPresentFrameDualFillNV' video_slot minPresentTime beginPresentTimeId presentDurationId type target0 fill0 target1 fill1 target2 fill2 target3 fill3@


glPresentFrameDualFillNV :: MonadIO m => GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> m ()
glPresentFrameDualFillNV = ffiuintuint64EXTuintuintenumenumuintenumuintenumuintenumuintIOV glPresentFrameDualFillNVFunPtr

glPresentFrameDualFillNVFunPtr :: FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> IO ())
glPresentFrameDualFillNVFunPtr = unsafePerformIO (getProcAddress "glPresentFrameDualFillNV")

{-# NOINLINE glPresentFrameDualFillNVFunPtr #-}

-- | Usage: @'glPresentFrameKeyedNV' video_slot minPresentTime beginPresentTimeId presentDurationId type target0 fill0 key0 target1 fill1 key1@


glPresentFrameKeyedNV :: MonadIO m => GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLuint -> GLenum -> GLuint -> GLuint -> m ()
glPresentFrameKeyedNV = ffiuintuint64EXTuintuintenumenumuintuintenumuintuintIOV glPresentFrameKeyedNVFunPtr

glPresentFrameKeyedNVFunPtr :: FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLuint -> GLenum -> GLuint -> GLuint -> IO ())
glPresentFrameKeyedNVFunPtr = unsafePerformIO (getProcAddress "glPresentFrameKeyedNV")

{-# NOINLINE glPresentFrameKeyedNVFunPtr #-}

pattern GL_CURRENT_TIME_NV  :: (Eq a, Num a) => a

pattern GL_CURRENT_TIME_NV = 0x8E28

pattern GL_FIELDS_NV  :: (Eq a, Num a) => a

pattern GL_FIELDS_NV = 0x8E27

pattern GL_FRAME_NV  :: (Eq a, Num a) => a

pattern GL_FRAME_NV = 0x8E26

pattern GL_NUM_FILL_STREAMS_NV  :: (Eq a, Num a) => a

pattern GL_NUM_FILL_STREAMS_NV = 0x8E29

pattern GL_PRESENT_DURATION_NV  :: (Eq a, Num a) => a

pattern GL_PRESENT_DURATION_NV = 0x8E2B

pattern GL_PRESENT_TIME_NV  :: (Eq a, Num a) => a

pattern GL_PRESENT_TIME_NV = 0x8E2A