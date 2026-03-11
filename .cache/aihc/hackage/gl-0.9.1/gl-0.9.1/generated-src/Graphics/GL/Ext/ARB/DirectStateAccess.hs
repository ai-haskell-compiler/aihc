-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DirectStateAccess (
  -- * Extension Support
    gl_ARB_direct_state_access

  -- * GL_ARB_direct_state_access
  , glBindTextureUnit
  , glBlitNamedFramebuffer
  , glCheckNamedFramebufferStatus
  , glClearNamedBufferData
  , glClearNamedBufferSubData
  , glClearNamedFramebufferfi
  , glClearNamedFramebufferfv
  , glClearNamedFramebufferiv
  , glClearNamedFramebufferuiv
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
  , glGetTransformFeedbacki64_v
  , glGetTransformFeedbacki_v
  , glGetTransformFeedbackiv
  , glGetVertexArrayIndexed64iv
  , glGetVertexArrayIndexediv
  , glGetVertexArrayiv
  , glInvalidateNamedFramebufferData
  , glInvalidateNamedFramebufferSubData
  , glMapNamedBuffer
  , glMapNamedBufferRange
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
  , pattern GL_QUERY_TARGET
  , pattern GL_TEXTURE_BINDING_1D
  , pattern GL_TEXTURE_BINDING_1D_ARRAY
  , pattern GL_TEXTURE_BINDING_2D
  , pattern GL_TEXTURE_BINDING_2D_ARRAY
  , pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE
  , pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
  , pattern GL_TEXTURE_BINDING_3D
  , pattern GL_TEXTURE_BINDING_BUFFER
  , pattern GL_TEXTURE_BINDING_CUBE_MAP
  , pattern GL_TEXTURE_BINDING_CUBE_MAP_ARRAY
  , pattern GL_TEXTURE_BINDING_RECTANGLE
  , pattern GL_TEXTURE_TARGET
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/direct_state_access.txt GL_ARB_direct_state_access> extension is available.

gl_ARB_direct_state_access :: Bool
gl_ARB_direct_state_access = member "GL_ARB_direct_state_access" extensions
{-# NOINLINE gl_ARB_direct_state_access #-}