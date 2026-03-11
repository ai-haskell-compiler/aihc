-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.ExtendedGet (
  -- * Extension Support
    gl_QCOM_extended_get

  -- * GL_QCOM_extended_get
  , glExtGetBufferPointervQCOM
  , glExtGetBuffersQCOM
  , glExtGetFramebuffersQCOM
  , glExtGetRenderbuffersQCOM
  , glExtGetTexLevelParameterivQCOM
  , glExtGetTexSubImageQCOM
  , glExtGetTexturesQCOM
  , glExtTexObjectStateOverrideiQCOM
  , pattern GL_STATE_RESTORE
  , pattern GL_TEXTURE_DEPTH_QCOM
  , pattern GL_TEXTURE_FORMAT_QCOM
  , pattern GL_TEXTURE_HEIGHT_QCOM
  , pattern GL_TEXTURE_IMAGE_VALID_QCOM
  , pattern GL_TEXTURE_INTERNAL_FORMAT_QCOM
  , pattern GL_TEXTURE_NUM_LEVELS_QCOM
  , pattern GL_TEXTURE_OBJECT_VALID_QCOM
  , pattern GL_TEXTURE_TARGET_QCOM
  , pattern GL_TEXTURE_TYPE_QCOM
  , pattern GL_TEXTURE_WIDTH_QCOM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/QCOM/QCOM_extended_get.txt GL_QCOM_extended_get> extension is available.

gl_QCOM_extended_get :: Bool
gl_QCOM_extended_get = member "GL_QCOM_extended_get" extensions
{-# NOINLINE gl_QCOM_extended_get #-}

-- | Usage: @'glExtGetBufferPointervQCOM' target params@


glExtGetBufferPointervQCOM :: MonadIO m => GLenum -> Ptr (Ptr ()) -> m ()
glExtGetBufferPointervQCOM = ffienumPtrPtrVIOV glExtGetBufferPointervQCOMFunPtr

glExtGetBufferPointervQCOMFunPtr :: FunPtr (GLenum -> Ptr (Ptr ()) -> IO ())
glExtGetBufferPointervQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetBufferPointervQCOM")

{-# NOINLINE glExtGetBufferPointervQCOMFunPtr #-}

-- | Usage: @'glExtGetBuffersQCOM' buffers maxBuffers numBuffers@
--
-- The length of @buffers@ should be @maxBuffers@.
--
-- The length of @numBuffers@ should be @1@.


glExtGetBuffersQCOM :: MonadIO m => Ptr GLuint -> GLint -> Ptr GLint -> m ()
glExtGetBuffersQCOM = ffiPtruintintPtrintIOV glExtGetBuffersQCOMFunPtr

glExtGetBuffersQCOMFunPtr :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
glExtGetBuffersQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetBuffersQCOM")

{-# NOINLINE glExtGetBuffersQCOMFunPtr #-}

-- | Usage: @'glExtGetFramebuffersQCOM' framebuffers maxFramebuffers numFramebuffers@
--
-- The length of @framebuffers@ should be @maxFramebuffers@.
--
-- The length of @numFramebuffers@ should be @1@.


glExtGetFramebuffersQCOM :: MonadIO m => Ptr GLuint -> GLint -> Ptr GLint -> m ()
glExtGetFramebuffersQCOM = ffiPtruintintPtrintIOV glExtGetFramebuffersQCOMFunPtr

glExtGetFramebuffersQCOMFunPtr :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
glExtGetFramebuffersQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetFramebuffersQCOM")

{-# NOINLINE glExtGetFramebuffersQCOMFunPtr #-}

-- | Usage: @'glExtGetRenderbuffersQCOM' renderbuffers maxRenderbuffers numRenderbuffers@
--
-- The length of @renderbuffers@ should be @maxRenderbuffers@.
--
-- The length of @numRenderbuffers@ should be @1@.


glExtGetRenderbuffersQCOM :: MonadIO m => Ptr GLuint -> GLint -> Ptr GLint -> m ()
glExtGetRenderbuffersQCOM = ffiPtruintintPtrintIOV glExtGetRenderbuffersQCOMFunPtr

glExtGetRenderbuffersQCOMFunPtr :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
glExtGetRenderbuffersQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetRenderbuffersQCOM")

{-# NOINLINE glExtGetRenderbuffersQCOMFunPtr #-}

-- | Usage: @'glExtGetTexLevelParameterivQCOM' texture face level pname params@


glExtGetTexLevelParameterivQCOM :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> m ()
glExtGetTexLevelParameterivQCOM = ffiuintenumintenumPtrintIOV glExtGetTexLevelParameterivQCOMFunPtr

glExtGetTexLevelParameterivQCOMFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
glExtGetTexLevelParameterivQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetTexLevelParameterivQCOM")

{-# NOINLINE glExtGetTexLevelParameterivQCOMFunPtr #-}

-- | Usage: @'glExtGetTexSubImageQCOM' target level xoffset yoffset zoffset width height depth format type texels@


glExtGetTexSubImageQCOM :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glExtGetTexSubImageQCOM = ffienumintintintintsizeisizeisizeienumenumPtrVIOV glExtGetTexSubImageQCOMFunPtr

glExtGetTexSubImageQCOMFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glExtGetTexSubImageQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetTexSubImageQCOM")

{-# NOINLINE glExtGetTexSubImageQCOMFunPtr #-}

-- | Usage: @'glExtGetTexturesQCOM' textures maxTextures numTextures@


glExtGetTexturesQCOM :: MonadIO m => Ptr GLuint -> GLint -> Ptr GLint -> m ()
glExtGetTexturesQCOM = ffiPtruintintPtrintIOV glExtGetTexturesQCOMFunPtr

glExtGetTexturesQCOMFunPtr :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
glExtGetTexturesQCOMFunPtr = unsafePerformIO (getProcAddress "glExtGetTexturesQCOM")

{-# NOINLINE glExtGetTexturesQCOMFunPtr #-}

-- | Usage: @'glExtTexObjectStateOverrideiQCOM' target pname param@


glExtTexObjectStateOverrideiQCOM :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glExtTexObjectStateOverrideiQCOM = ffienumenumintIOV glExtTexObjectStateOverrideiQCOMFunPtr

glExtTexObjectStateOverrideiQCOMFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glExtTexObjectStateOverrideiQCOMFunPtr = unsafePerformIO (getProcAddress "glExtTexObjectStateOverrideiQCOM")

{-# NOINLINE glExtTexObjectStateOverrideiQCOMFunPtr #-}

pattern GL_STATE_RESTORE  :: (Eq a, Num a) => a

pattern GL_STATE_RESTORE = 0x8BDC

pattern GL_TEXTURE_DEPTH_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_DEPTH_QCOM = 0x8BD4

pattern GL_TEXTURE_FORMAT_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_FORMAT_QCOM = 0x8BD6

pattern GL_TEXTURE_HEIGHT_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_HEIGHT_QCOM = 0x8BD3

pattern GL_TEXTURE_IMAGE_VALID_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_IMAGE_VALID_QCOM = 0x8BD8

pattern GL_TEXTURE_INTERNAL_FORMAT_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_INTERNAL_FORMAT_QCOM = 0x8BD5

pattern GL_TEXTURE_NUM_LEVELS_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_NUM_LEVELS_QCOM = 0x8BD9

pattern GL_TEXTURE_OBJECT_VALID_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_OBJECT_VALID_QCOM = 0x8BDB

pattern GL_TEXTURE_TARGET_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_TARGET_QCOM = 0x8BDA

pattern GL_TEXTURE_TYPE_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_TYPE_QCOM = 0x8BD7

pattern GL_TEXTURE_WIDTH_QCOM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_WIDTH_QCOM = 0x8BD2