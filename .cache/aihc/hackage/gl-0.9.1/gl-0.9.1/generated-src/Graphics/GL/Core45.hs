-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Core45 (
  -- * Graphics.GL.Core45
    module Graphics.GL.Core44
  , glBindTextureUnit
  , glBlitNamedFramebuffer
  , glCheckNamedFramebufferStatus
  , glClearNamedBufferData
  , glClearNamedBufferSubData
  , glClearNamedFramebufferfi
  , glClearNamedFramebufferfv
  , glClearNamedFramebufferiv
  , glClearNamedFramebufferuiv
  , glClipControl
  , glCompressedTextureSubImage1D
  , glCompressedTextureSubImage2D
  , glCompressedTextureSubImage3D
  , glCopyNamedBufferSubData
  , glCopyTextureSubImage1D
  , glCopyTextureSubImage2D
  , glCopyTextureSubImage3D
  , glCreateBuffers
  , glCreateFramebuffers
  , glCreateProgramPipelines
  , glCreateQueries
  , glCreateRenderbuffers
  , glCreateSamplers
  , glCreateTextures
  , glCreateTransformFeedbacks
  , glCreateVertexArrays
  , glDisableVertexArrayAttrib
  , glEnableVertexArrayAttrib
  , glFlushMappedNamedBufferRange
  , glGenerateTextureMipmap
  , glGetCompressedTextureImage
  , glGetCompressedTextureSubImage
  , glGetGraphicsResetStatus
  , glGetNamedBufferParameteri64v
  , glGetNamedBufferParameteriv
  , glGetNamedBufferPointerv
  , glGetNamedBufferSubData
  , glGetNamedFramebufferAttachmentParameteriv
  , glGetNamedFramebufferParameteriv
  , glGetNamedRenderbufferParameteriv
  , glGetQueryBufferObjecti64v
  , glGetQueryBufferObjectiv
  , glGetQueryBufferObjectui64v
  , glGetQueryBufferObjectuiv
  , glGetTextureImage
  , glGetTextureLevelParameterfv
  , glGetTextureLevelParameteriv
  , glGetTextureParameterIiv
  , glGetTextureParameterIuiv
  , glGetTextureParameterfv
  , glGetTextureParameteriv
  , glGetTextureSubImage
  , glGetTransformFeedbacki64_v
  , glGetTransformFeedbacki_v
  , glGetTransformFeedbackiv
  , glGetVertexArrayIndexed64iv
  , glGetVertexArrayIndexediv
  , glGetVertexArrayiv
  , glGetnCompressedTexImage
  , glGetnTexImage
  , glGetnUniformdv
  , glGetnUniformfv
  , glGetnUniformiv
  , glGetnUniformuiv
  , glInvalidateNamedFramebufferData
  , glInvalidateNamedFramebufferSubData
  , glMapNamedBuffer
  , glMapNamedBufferRange
  , glMemoryBarrierByRegion
  , glNamedBufferData
  , glNamedBufferStorage
  , glNamedBufferSubData
  , glNamedFramebufferDrawBuffer
  , glNamedFramebufferDrawBuffers
  , glNamedFramebufferParameteri
  , glNamedFramebufferReadBuffer
  , glNamedFramebufferRenderbuffer
  , glNamedFramebufferTexture
  , glNamedFramebufferTextureLayer
  , glNamedRenderbufferStorage
  , glNamedRenderbufferStorageMultisample
  , glReadnPixels
  , glTextureBarrier
  , glTextureBuffer
  , glTextureBufferRange
  , glTextureParameterIiv
  , glTextureParameterIuiv
  , glTextureParameterf
  , glTextureParameterfv
  , glTextureParameteri
  , glTextureParameteriv
  , glTextureStorage1D
  , glTextureStorage2D
  , glTextureStorage2DMultisample
  , glTextureStorage3D
  , glTextureStorage3DMultisample
  , glTextureSubImage1D
  , glTextureSubImage2D
  , glTextureSubImage3D
  , glTransformFeedbackBufferBase
  , glTransformFeedbackBufferRange
  , glUnmapNamedBuffer
  , glVertexArrayAttribBinding
  , glVertexArrayAttribFormat
  , glVertexArrayAttribIFormat
  , glVertexArrayAttribLFormat
  , glVertexArrayBindingDivisor
  , glVertexArrayElementBuffer
  , glVertexArrayVertexBuffer
  , glVertexArrayVertexBuffers
  , pattern GL_CLIP_DEPTH_MODE
  , pattern GL_CLIP_ORIGIN
  , pattern GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT
  , pattern GL_CONTEXT_LOST
  , pattern GL_CONTEXT_RELEASE_BEHAVIOR
  , pattern GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH
  , pattern GL_GUILTY_CONTEXT_RESET
  , pattern GL_INNOCENT_CONTEXT_RESET
  , pattern GL_LOSE_CONTEXT_ON_RESET
  , pattern GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES
  , pattern GL_MAX_CULL_DISTANCES
  , pattern GL_NEGATIVE_ONE_TO_ONE
  , pattern GL_NO_RESET_NOTIFICATION
  , pattern GL_QUERY_BY_REGION_NO_WAIT_INVERTED
  , pattern GL_QUERY_BY_REGION_WAIT_INVERTED
  , pattern GL_QUERY_NO_WAIT_INVERTED
  , pattern GL_QUERY_TARGET
  , pattern GL_QUERY_WAIT_INVERTED
  , pattern GL_RESET_NOTIFICATION_STRATEGY
  , pattern GL_TEXTURE_TARGET
  , pattern GL_UNKNOWN_CONTEXT_RESET
  , pattern GL_ZERO_TO_ONE
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

import Graphics.GL.Core44

-- | Usage: @'glGetnCompressedTexImage' target lod bufSize pixels@


glGetnCompressedTexImage :: MonadIO m => GLenum -> GLint -> GLsizei -> Ptr () -> m ()
glGetnCompressedTexImage = ffienumintsizeiPtrVIOV glGetnCompressedTexImageFunPtr

glGetnCompressedTexImageFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> Ptr () -> IO ())
glGetnCompressedTexImageFunPtr = unsafePerformIO (getProcAddress "glGetnCompressedTexImage")

{-# NOINLINE glGetnCompressedTexImageFunPtr #-}

-- | Usage: @'glGetnTexImage' target level format type bufSize pixels@


glGetnTexImage :: MonadIO m => GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glGetnTexImage = ffienumintenumenumsizeiPtrVIOV glGetnTexImageFunPtr

glGetnTexImageFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glGetnTexImageFunPtr = unsafePerformIO (getProcAddress "glGetnTexImage")

{-# NOINLINE glGetnTexImageFunPtr #-}

-- | Usage: @'glGetnUniformdv' program location bufSize params@


glGetnUniformdv :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLdouble -> m ()
glGetnUniformdv = ffiuintintsizeiPtrdoubleIOV glGetnUniformdvFunPtr

glGetnUniformdvFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
glGetnUniformdvFunPtr = unsafePerformIO (getProcAddress "glGetnUniformdv")

{-# NOINLINE glGetnUniformdvFunPtr #-}