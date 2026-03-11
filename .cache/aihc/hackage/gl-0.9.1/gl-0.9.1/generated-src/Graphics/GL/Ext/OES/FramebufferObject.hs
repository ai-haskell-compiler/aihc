-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.FramebufferObject (
  -- * Extension Support
    gl_OES_framebuffer_object

  -- * GL_OES_framebuffer_object
  , glBindFramebufferOES
  , glBindRenderbufferOES
  , glCheckFramebufferStatusOES
  , glDeleteFramebuffersOES
  , glDeleteRenderbuffersOES
  , glFramebufferRenderbufferOES
  , glFramebufferTexture2DOES
  , glGenFramebuffersOES
  , glGenRenderbuffersOES
  , glGenerateMipmapOES
  , glGetFramebufferAttachmentParameterivOES
  , glGetRenderbufferParameterivOES
  , glIsFramebufferOES
  , glIsRenderbufferOES
  , glRenderbufferStorageOES
  , pattern GL_COLOR_ATTACHMENT0_OES
  , pattern GL_DEPTH_ATTACHMENT_OES
  , pattern GL_DEPTH_COMPONENT16_OES
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_OES
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_OES
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_OES
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_OES
  , pattern GL_FRAMEBUFFER_BINDING_OES
  , pattern GL_FRAMEBUFFER_COMPLETE_OES
  , pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_OES
  , pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_OES
  , pattern GL_FRAMEBUFFER_INCOMPLETE_FORMATS_OES
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_OES
  , pattern GL_FRAMEBUFFER_OES
  , pattern GL_FRAMEBUFFER_UNSUPPORTED_OES
  , pattern GL_INVALID_FRAMEBUFFER_OPERATION_OES
  , pattern GL_MAX_RENDERBUFFER_SIZE_OES
  , pattern GL_NONE_OES
  , pattern GL_RENDERBUFFER_ALPHA_SIZE_OES
  , pattern GL_RENDERBUFFER_BINDING_OES
  , pattern GL_RENDERBUFFER_BLUE_SIZE_OES
  , pattern GL_RENDERBUFFER_DEPTH_SIZE_OES
  , pattern GL_RENDERBUFFER_GREEN_SIZE_OES
  , pattern GL_RENDERBUFFER_HEIGHT_OES
  , pattern GL_RENDERBUFFER_INTERNAL_FORMAT_OES
  , pattern GL_RENDERBUFFER_OES
  , pattern GL_RENDERBUFFER_RED_SIZE_OES
  , pattern GL_RENDERBUFFER_STENCIL_SIZE_OES
  , pattern GL_RENDERBUFFER_WIDTH_OES
  , pattern GL_RGB565_OES
  , pattern GL_RGB5_A1_OES
  , pattern GL_RGBA4_OES
  , pattern GL_STENCIL_ATTACHMENT_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_framebuffer_object.txt GL_OES_framebuffer_object> extension is available.

gl_OES_framebuffer_object :: Bool
gl_OES_framebuffer_object = member "GL_OES_framebuffer_object" extensions
{-# NOINLINE gl_OES_framebuffer_object #-}

-- | Usage: @'glBindFramebufferOES' target framebuffer@


glBindFramebufferOES :: MonadIO m => GLenum -> GLuint -> m ()
glBindFramebufferOES = ffienumuintIOV glBindFramebufferOESFunPtr

glBindFramebufferOESFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindFramebufferOESFunPtr = unsafePerformIO (getProcAddress "glBindFramebufferOES")

{-# NOINLINE glBindFramebufferOESFunPtr #-}

-- | Usage: @'glBindRenderbufferOES' target renderbuffer@


glBindRenderbufferOES :: MonadIO m => GLenum -> GLuint -> m ()
glBindRenderbufferOES = ffienumuintIOV glBindRenderbufferOESFunPtr

glBindRenderbufferOESFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindRenderbufferOESFunPtr = unsafePerformIO (getProcAddress "glBindRenderbufferOES")

{-# NOINLINE glBindRenderbufferOESFunPtr #-}

-- | Usage: @'glCheckFramebufferStatusOES' target@


glCheckFramebufferStatusOES :: MonadIO m => GLenum -> m GLenum
glCheckFramebufferStatusOES = ffienumIOenum glCheckFramebufferStatusOESFunPtr

glCheckFramebufferStatusOESFunPtr :: FunPtr (GLenum -> IO GLenum)
glCheckFramebufferStatusOESFunPtr = unsafePerformIO (getProcAddress "glCheckFramebufferStatusOES")

{-# NOINLINE glCheckFramebufferStatusOESFunPtr #-}

-- | Usage: @'glDeleteFramebuffersOES' n framebuffers@
--
-- The length of @framebuffers@ should be @n@.


glDeleteFramebuffersOES :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteFramebuffersOES = ffisizeiPtruintIOV glDeleteFramebuffersOESFunPtr

glDeleteFramebuffersOESFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteFramebuffersOESFunPtr = unsafePerformIO (getProcAddress "glDeleteFramebuffersOES")

{-# NOINLINE glDeleteFramebuffersOESFunPtr #-}

-- | Usage: @'glDeleteRenderbuffersOES' n renderbuffers@
--
-- The length of @renderbuffers@ should be @n@.


glDeleteRenderbuffersOES :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteRenderbuffersOES = ffisizeiPtruintIOV glDeleteRenderbuffersOESFunPtr

glDeleteRenderbuffersOESFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteRenderbuffersOESFunPtr = unsafePerformIO (getProcAddress "glDeleteRenderbuffersOES")

{-# NOINLINE glDeleteRenderbuffersOESFunPtr #-}

-- | Usage: @'glFramebufferRenderbufferOES' target attachment renderbuffertarget renderbuffer@


glFramebufferRenderbufferOES :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> m ()
glFramebufferRenderbufferOES = ffienumenumenumuintIOV glFramebufferRenderbufferOESFunPtr

glFramebufferRenderbufferOESFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
glFramebufferRenderbufferOESFunPtr = unsafePerformIO (getProcAddress "glFramebufferRenderbufferOES")

{-# NOINLINE glFramebufferRenderbufferOESFunPtr #-}

-- | Usage: @'glFramebufferTexture2DOES' target attachment textarget texture level@


glFramebufferTexture2DOES :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> m ()
glFramebufferTexture2DOES = ffienumenumenumuintintIOV glFramebufferTexture2DOESFunPtr

glFramebufferTexture2DOESFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
glFramebufferTexture2DOESFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture2DOES")

{-# NOINLINE glFramebufferTexture2DOESFunPtr #-}

-- | Usage: @'glGenFramebuffersOES' n framebuffers@
--
-- The length of @framebuffers@ should be @n@.


glGenFramebuffersOES :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenFramebuffersOES = ffisizeiPtruintIOV glGenFramebuffersOESFunPtr

glGenFramebuffersOESFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenFramebuffersOESFunPtr = unsafePerformIO (getProcAddress "glGenFramebuffersOES")

{-# NOINLINE glGenFramebuffersOESFunPtr #-}

-- | Usage: @'glGenRenderbuffersOES' n renderbuffers@
--
-- The length of @renderbuffers@ should be @n@.


glGenRenderbuffersOES :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenRenderbuffersOES = ffisizeiPtruintIOV glGenRenderbuffersOESFunPtr

glGenRenderbuffersOESFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenRenderbuffersOESFunPtr = unsafePerformIO (getProcAddress "glGenRenderbuffersOES")

{-# NOINLINE glGenRenderbuffersOESFunPtr #-}

-- | Usage: @'glGenerateMipmapOES' target@


glGenerateMipmapOES :: MonadIO m => GLenum -> m ()
glGenerateMipmapOES = ffienumIOV glGenerateMipmapOESFunPtr

glGenerateMipmapOESFunPtr :: FunPtr (GLenum -> IO ())
glGenerateMipmapOESFunPtr = unsafePerformIO (getProcAddress "glGenerateMipmapOES")

{-# NOINLINE glGenerateMipmapOESFunPtr #-}

-- | Usage: @'glGetFramebufferAttachmentParameterivOES' target attachment pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFramebufferAttachmentParameterivOES :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetFramebufferAttachmentParameterivOES = ffienumenumenumPtrintIOV glGetFramebufferAttachmentParameterivOESFunPtr

glGetFramebufferAttachmentParameterivOESFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetFramebufferAttachmentParameterivOESFunPtr = unsafePerformIO (getProcAddress "glGetFramebufferAttachmentParameterivOES")

{-# NOINLINE glGetFramebufferAttachmentParameterivOESFunPtr #-}

-- | Usage: @'glGetRenderbufferParameterivOES' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetRenderbufferParameterivOES :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetRenderbufferParameterivOES = ffienumenumPtrintIOV glGetRenderbufferParameterivOESFunPtr

glGetRenderbufferParameterivOESFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetRenderbufferParameterivOESFunPtr = unsafePerformIO (getProcAddress "glGetRenderbufferParameterivOES")

{-# NOINLINE glGetRenderbufferParameterivOESFunPtr #-}

-- | Usage: @'glIsFramebufferOES' framebuffer@


glIsFramebufferOES :: MonadIO m => GLuint -> m GLboolean
glIsFramebufferOES = ffiuintIOboolean glIsFramebufferOESFunPtr

glIsFramebufferOESFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsFramebufferOESFunPtr = unsafePerformIO (getProcAddress "glIsFramebufferOES")

{-# NOINLINE glIsFramebufferOESFunPtr #-}

-- | Usage: @'glIsRenderbufferOES' renderbuffer@


glIsRenderbufferOES :: MonadIO m => GLuint -> m GLboolean
glIsRenderbufferOES = ffiuintIOboolean glIsRenderbufferOESFunPtr

glIsRenderbufferOESFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsRenderbufferOESFunPtr = unsafePerformIO (getProcAddress "glIsRenderbufferOES")

{-# NOINLINE glIsRenderbufferOESFunPtr #-}

-- | Usage: @'glRenderbufferStorageOES' target internalformat width height@


glRenderbufferStorageOES :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLsizei -> m ()
glRenderbufferStorageOES = ffienumenumsizeisizeiIOV glRenderbufferStorageOESFunPtr

glRenderbufferStorageOESFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
glRenderbufferStorageOESFunPtr = unsafePerformIO (getProcAddress "glRenderbufferStorageOES")

{-# NOINLINE glRenderbufferStorageOESFunPtr #-}

pattern GL_COLOR_ATTACHMENT0_OES  :: (Eq a, Num a) => a

pattern GL_COLOR_ATTACHMENT0_OES = 0x8CE0

pattern GL_DEPTH_ATTACHMENT_OES  :: (Eq a, Num a) => a

pattern GL_DEPTH_ATTACHMENT_OES = 0x8D00

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_OES = 0x8CD1

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_OES = 0x8CD0

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_OES = 0x8CD3

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_OES = 0x8CD2

pattern GL_FRAMEBUFFER_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_BINDING_OES = 0x8CA6

pattern GL_FRAMEBUFFER_COMPLETE_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_COMPLETE_OES = 0x8CD5

pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_OES = 0x8CD6

pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_OES = 0x8CD9

pattern GL_FRAMEBUFFER_INCOMPLETE_FORMATS_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_FORMATS_OES = 0x8CDA

pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_OES = 0x8CD7

pattern GL_FRAMEBUFFER_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_OES = 0x8D40

pattern GL_FRAMEBUFFER_UNSUPPORTED_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_UNSUPPORTED_OES = 0x8CDD

pattern GL_INVALID_FRAMEBUFFER_OPERATION_OES  :: (Eq a, Num a) => a

pattern GL_INVALID_FRAMEBUFFER_OPERATION_OES = 0x0506

pattern GL_MAX_RENDERBUFFER_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_MAX_RENDERBUFFER_SIZE_OES = 0x84E8

pattern GL_NONE_OES  :: (Eq a, Num a) => a

pattern GL_NONE_OES = 0

pattern GL_RENDERBUFFER_ALPHA_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_ALPHA_SIZE_OES = 0x8D53

pattern GL_RENDERBUFFER_BINDING_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_BINDING_OES = 0x8CA7

pattern GL_RENDERBUFFER_BLUE_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_BLUE_SIZE_OES = 0x8D52

pattern GL_RENDERBUFFER_DEPTH_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_DEPTH_SIZE_OES = 0x8D54

pattern GL_RENDERBUFFER_GREEN_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_GREEN_SIZE_OES = 0x8D51

pattern GL_RENDERBUFFER_HEIGHT_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_HEIGHT_OES = 0x8D43

pattern GL_RENDERBUFFER_INTERNAL_FORMAT_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_INTERNAL_FORMAT_OES = 0x8D44

pattern GL_RENDERBUFFER_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_OES = 0x8D41

pattern GL_RENDERBUFFER_RED_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_RED_SIZE_OES = 0x8D50

pattern GL_RENDERBUFFER_STENCIL_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_STENCIL_SIZE_OES = 0x8D55

pattern GL_RENDERBUFFER_WIDTH_OES  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_WIDTH_OES = 0x8D42

pattern GL_STENCIL_ATTACHMENT_OES  :: (Eq a, Num a) => a

pattern GL_STENCIL_ATTACHMENT_OES = 0x8D20