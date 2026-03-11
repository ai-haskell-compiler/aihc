-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DirectStateAccess (
  -- * Extension Support
    gl_EXT_direct_state_access

  -- * GL_EXT_direct_state_access
  , glBindMultiTextureEXT
  , glCheckNamedFramebufferStatusEXT
  , glClearNamedBufferDataEXT
  , glClearNamedBufferSubDataEXT
  , glClientAttribDefaultEXT
  , glCompressedMultiTexImage1DEXT
  , glCompressedMultiTexImage2DEXT
  , glCompressedMultiTexImage3DEXT
  , glCompressedMultiTexSubImage1DEXT
  , glCompressedMultiTexSubImage2DEXT
  , glCompressedMultiTexSubImage3DEXT
  , glCompressedTextureImage1DEXT
  , glCompressedTextureImage2DEXT
  , glCompressedTextureImage3DEXT
  , glCompressedTextureSubImage1DEXT
  , glCompressedTextureSubImage2DEXT
  , glCompressedTextureSubImage3DEXT
  , glCopyMultiTexImage1DEXT
  , glCopyMultiTexImage2DEXT
  , glCopyMultiTexSubImage1DEXT
  , glCopyMultiTexSubImage2DEXT
  , glCopyMultiTexSubImage3DEXT
  , glCopyTextureImage1DEXT
  , glCopyTextureImage2DEXT
  , glCopyTextureSubImage1DEXT
  , glCopyTextureSubImage2DEXT
  , glCopyTextureSubImage3DEXT
  , glDisableClientStateIndexedEXT
  , glDisableClientStateiEXT
  , glDisableIndexedEXT
  , glDisableVertexArrayAttribEXT
  , glDisableVertexArrayEXT
  , glEnableClientStateIndexedEXT
  , glEnableClientStateiEXT
  , glEnableIndexedEXT
  , glEnableVertexArrayAttribEXT
  , glEnableVertexArrayEXT
  , glFlushMappedNamedBufferRangeEXT
  , glFramebufferDrawBufferEXT
  , glFramebufferDrawBuffersEXT
  , glFramebufferReadBufferEXT
  , glGenerateMultiTexMipmapEXT
  , glGenerateTextureMipmapEXT
  , glGetBooleanIndexedvEXT
  , glGetCompressedMultiTexImageEXT
  , glGetCompressedTextureImageEXT
  , glGetDoubleIndexedvEXT
  , glGetDoublei_vEXT
  , glGetFloatIndexedvEXT
  , glGetFloati_vEXT
  , glGetFramebufferParameterivEXT
  , glGetIntegerIndexedvEXT
  , glGetMultiTexEnvfvEXT
  , glGetMultiTexEnvivEXT
  , glGetMultiTexGendvEXT
  , glGetMultiTexGenfvEXT
  , glGetMultiTexGenivEXT
  , glGetMultiTexImageEXT
  , glGetMultiTexLevelParameterfvEXT
  , glGetMultiTexLevelParameterivEXT
  , glGetMultiTexParameterIivEXT
  , glGetMultiTexParameterIuivEXT
  , glGetMultiTexParameterfvEXT
  , glGetMultiTexParameterivEXT
  , glGetNamedBufferParameterivEXT
  , glGetNamedBufferPointervEXT
  , glGetNamedBufferSubDataEXT
  , glGetNamedFramebufferAttachmentParameterivEXT
  , glGetNamedFramebufferParameterivEXT
  , glGetNamedProgramLocalParameterIivEXT
  , glGetNamedProgramLocalParameterIuivEXT
  , glGetNamedProgramLocalParameterdvEXT
  , glGetNamedProgramLocalParameterfvEXT
  , glGetNamedProgramStringEXT
  , glGetNamedProgramivEXT
  , glGetNamedRenderbufferParameterivEXT
  , glGetPointerIndexedvEXT
  , glGetPointeri_vEXT
  , glGetTextureImageEXT
  , glGetTextureLevelParameterfvEXT
  , glGetTextureLevelParameterivEXT
  , glGetTextureParameterIivEXT
  , glGetTextureParameterIuivEXT
  , glGetTextureParameterfvEXT
  , glGetTextureParameterivEXT
  , glGetVertexArrayIntegeri_vEXT
  , glGetVertexArrayIntegervEXT
  , glGetVertexArrayPointeri_vEXT
  , glGetVertexArrayPointervEXT
  , glIsEnabledIndexedEXT
  , glMapNamedBufferEXT
  , glMapNamedBufferRangeEXT
  , glMatrixFrustumEXT
  , glMatrixLoadIdentityEXT
  , glMatrixLoadTransposedEXT
  , glMatrixLoadTransposefEXT
  , glMatrixLoaddEXT
  , glMatrixLoadfEXT
  , glMatrixMultTransposedEXT
  , glMatrixMultTransposefEXT
  , glMatrixMultdEXT
  , glMatrixMultfEXT
  , glMatrixOrthoEXT
  , glMatrixPopEXT
  , glMatrixPushEXT
  , glMatrixRotatedEXT
  , glMatrixRotatefEXT
  , glMatrixScaledEXT
  , glMatrixScalefEXT
  , glMatrixTranslatedEXT
  , glMatrixTranslatefEXT
  , glMultiTexBufferEXT
  , glMultiTexCoordPointerEXT
  , glMultiTexEnvfEXT
  , glMultiTexEnvfvEXT
  , glMultiTexEnviEXT
  , glMultiTexEnvivEXT
  , glMultiTexGendEXT
  , glMultiTexGendvEXT
  , glMultiTexGenfEXT
  , glMultiTexGenfvEXT
  , glMultiTexGeniEXT
  , glMultiTexGenivEXT
  , glMultiTexImage1DEXT
  , glMultiTexImage2DEXT
  , glMultiTexImage3DEXT
  , glMultiTexParameterIivEXT
  , glMultiTexParameterIuivEXT
  , glMultiTexParameterfEXT
  , glMultiTexParameterfvEXT
  , glMultiTexParameteriEXT
  , glMultiTexParameterivEXT
  , glMultiTexRenderbufferEXT
  , glMultiTexSubImage1DEXT
  , glMultiTexSubImage2DEXT
  , glMultiTexSubImage3DEXT
  , glNamedBufferDataEXT
  , glNamedBufferStorageEXT
  , glNamedBufferSubDataEXT
  , glNamedCopyBufferSubDataEXT
  , glNamedFramebufferParameteriEXT
  , glNamedFramebufferRenderbufferEXT
  , glNamedFramebufferTexture1DEXT
  , glNamedFramebufferTexture2DEXT
  , glNamedFramebufferTexture3DEXT
  , glNamedFramebufferTextureEXT
  , glNamedFramebufferTextureFaceEXT
  , glNamedFramebufferTextureLayerEXT
  , glNamedProgramLocalParameter4dEXT
  , glNamedProgramLocalParameter4dvEXT
  , glNamedProgramLocalParameter4fEXT
  , glNamedProgramLocalParameter4fvEXT
  , glNamedProgramLocalParameterI4iEXT
  , glNamedProgramLocalParameterI4ivEXT
  , glNamedProgramLocalParameterI4uiEXT
  , glNamedProgramLocalParameterI4uivEXT
  , glNamedProgramLocalParameters4fvEXT
  , glNamedProgramLocalParametersI4ivEXT
  , glNamedProgramLocalParametersI4uivEXT
  , glNamedProgramStringEXT
  , glNamedRenderbufferStorageEXT
  , glNamedRenderbufferStorageMultisampleCoverageEXT
  , glNamedRenderbufferStorageMultisampleEXT
  , glProgramUniform1dEXT
  , glProgramUniform1dvEXT
  , glProgramUniform1fEXT
  , glProgramUniform1fvEXT
  , glProgramUniform1iEXT
  , glProgramUniform1ivEXT
  , glProgramUniform1uiEXT
  , glProgramUniform1uivEXT
  , glProgramUniform2dEXT
  , glProgramUniform2dvEXT
  , glProgramUniform2fEXT
  , glProgramUniform2fvEXT
  , glProgramUniform2iEXT
  , glProgramUniform2ivEXT
  , glProgramUniform2uiEXT
  , glProgramUniform2uivEXT
  , glProgramUniform3dEXT
  , glProgramUniform3dvEXT
  , glProgramUniform3fEXT
  , glProgramUniform3fvEXT
  , glProgramUniform3iEXT
  , glProgramUniform3ivEXT
  , glProgramUniform3uiEXT
  , glProgramUniform3uivEXT
  , glProgramUniform4dEXT
  , glProgramUniform4dvEXT
  , glProgramUniform4fEXT
  , glProgramUniform4fvEXT
  , glProgramUniform4iEXT
  , glProgramUniform4ivEXT
  , glProgramUniform4uiEXT
  , glProgramUniform4uivEXT
  , glProgramUniformMatrix2dvEXT
  , glProgramUniformMatrix2fvEXT
  , glProgramUniformMatrix2x3dvEXT
  , glProgramUniformMatrix2x3fvEXT
  , glProgramUniformMatrix2x4dvEXT
  , glProgramUniformMatrix2x4fvEXT
  , glProgramUniformMatrix3dvEXT
  , glProgramUniformMatrix3fvEXT
  , glProgramUniformMatrix3x2dvEXT
  , glProgramUniformMatrix3x2fvEXT
  , glProgramUniformMatrix3x4dvEXT
  , glProgramUniformMatrix3x4fvEXT
  , glProgramUniformMatrix4dvEXT
  , glProgramUniformMatrix4fvEXT
  , glProgramUniformMatrix4x2dvEXT
  , glProgramUniformMatrix4x2fvEXT
  , glProgramUniformMatrix4x3dvEXT
  , glProgramUniformMatrix4x3fvEXT
  , glPushClientAttribDefaultEXT
  , glTextureBufferEXT
  , glTextureBufferRangeEXT
  , glTextureImage1DEXT
  , glTextureImage2DEXT
  , glTextureImage3DEXT
  , glTexturePageCommitmentEXT
  , glTextureParameterIivEXT
  , glTextureParameterIuivEXT
  , glTextureParameterfEXT
  , glTextureParameterfvEXT
  , glTextureParameteriEXT
  , glTextureParameterivEXT
  , glTextureRenderbufferEXT
  , glTextureStorage1DEXT
  , glTextureStorage2DEXT
  , glTextureStorage2DMultisampleEXT
  , glTextureStorage3DEXT
  , glTextureStorage3DMultisampleEXT
  , glTextureSubImage1DEXT
  , glTextureSubImage2DEXT
  , glTextureSubImage3DEXT
  , glUnmapNamedBufferEXT
  , glVertexArrayBindVertexBufferEXT
  , glVertexArrayColorOffsetEXT
  , glVertexArrayEdgeFlagOffsetEXT
  , glVertexArrayFogCoordOffsetEXT
  , glVertexArrayIndexOffsetEXT
  , glVertexArrayMultiTexCoordOffsetEXT
  , glVertexArrayNormalOffsetEXT
  , glVertexArraySecondaryColorOffsetEXT
  , glVertexArrayTexCoordOffsetEXT
  , glVertexArrayVertexAttribBindingEXT
  , glVertexArrayVertexAttribDivisorEXT
  , glVertexArrayVertexAttribFormatEXT
  , glVertexArrayVertexAttribIFormatEXT
  , glVertexArrayVertexAttribIOffsetEXT
  , glVertexArrayVertexAttribLFormatEXT
  , glVertexArrayVertexAttribLOffsetEXT
  , glVertexArrayVertexAttribOffsetEXT
  , glVertexArrayVertexBindingDivisorEXT
  , glVertexArrayVertexOffsetEXT
  , pattern GL_PROGRAM_MATRIX_EXT
  , pattern GL_PROGRAM_MATRIX_STACK_DEPTH_EXT
  , pattern GL_TRANSPOSE_PROGRAM_MATRIX_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/direct_state_access.txt GL_EXT_direct_state_access> extension is available.

gl_EXT_direct_state_access :: Bool
gl_EXT_direct_state_access = member "GL_EXT_direct_state_access" extensions
{-# NOINLINE gl_EXT_direct_state_access #-}

-- | Usage: @'glBindMultiTextureEXT' texunit target texture@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @texture@ is a @Texture@.


glBindMultiTextureEXT :: MonadIO m => GLenum -> GLenum -> GLuint -> m ()
glBindMultiTextureEXT = ffienumenumuintIOV glBindMultiTextureEXTFunPtr

glBindMultiTextureEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
glBindMultiTextureEXTFunPtr = unsafePerformIO (getProcAddress "glBindMultiTextureEXT")

{-# NOINLINE glBindMultiTextureEXTFunPtr #-}

-- | Usage: @'glCheckNamedFramebufferStatusEXT' framebuffer target@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @target@ is a @FramebufferTarget@.


glCheckNamedFramebufferStatusEXT :: MonadIO m => GLuint -> GLenum -> m GLenum
glCheckNamedFramebufferStatusEXT = ffiuintenumIOenum glCheckNamedFramebufferStatusEXTFunPtr

glCheckNamedFramebufferStatusEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO GLenum)
glCheckNamedFramebufferStatusEXTFunPtr = unsafePerformIO (getProcAddress "glCheckNamedFramebufferStatusEXT")

{-# NOINLINE glCheckNamedFramebufferStatusEXTFunPtr #-}

-- | Usage: @'glClearNamedBufferDataEXT' buffer internalformat format type data@
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @data@ should be @COMPSIZE(format,type)@.


glClearNamedBufferDataEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLenum -> Ptr () -> m ()
glClearNamedBufferDataEXT = ffiuintenumenumenumPtrVIOV glClearNamedBufferDataEXTFunPtr

glClearNamedBufferDataEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ())
glClearNamedBufferDataEXTFunPtr = unsafePerformIO (getProcAddress "glClearNamedBufferDataEXT")

{-# NOINLINE glClearNamedBufferDataEXTFunPtr #-}

-- | Usage: @'glClearNamedBufferSubDataEXT' buffer internalformat offset size format type data@
--
-- The parameter @offset@ is a @BufferSize@.
--
-- The parameter @size@ is a @BufferSize@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @data@ should be @COMPSIZE(format,type)@.


glClearNamedBufferSubDataEXT :: MonadIO m => GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> m ()
glClearNamedBufferSubDataEXT = ffiuintenumsizeiptrsizeiptrenumenumPtrVIOV glClearNamedBufferSubDataEXTFunPtr

glClearNamedBufferSubDataEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ())
glClearNamedBufferSubDataEXTFunPtr = unsafePerformIO (getProcAddress "glClearNamedBufferSubDataEXT")

{-# NOINLINE glClearNamedBufferSubDataEXTFunPtr #-}

-- | Usage: @'glClientAttribDefaultEXT' mask@
--
-- The parameter @mask@ is a @ClientAttribMask@, one of: 'Graphics.GL.Internal.Shared.GL_CLIENT_ALL_ATTRIB_BITS', 'Graphics.GL.Internal.Shared.GL_CLIENT_PIXEL_STORE_BIT', 'Graphics.GL.Internal.Shared.GL_CLIENT_VERTEX_ARRAY_BIT'.


glClientAttribDefaultEXT :: MonadIO m => GLbitfield -> m ()
glClientAttribDefaultEXT = ffibitfieldIOV glClientAttribDefaultEXTFunPtr

glClientAttribDefaultEXTFunPtr :: FunPtr (GLbitfield -> IO ())
glClientAttribDefaultEXTFunPtr = unsafePerformIO (getProcAddress "glClientAttribDefaultEXT")

{-# NOINLINE glClientAttribDefaultEXTFunPtr #-}

-- | Usage: @'glCompressedMultiTexImage1DEXT' texunit target level internalformat width border imageSize bits@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The length of @bits@ should be @imageSize@.


glCompressedMultiTexImage1DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
glCompressedMultiTexImage1DEXT = ffienumenumintenumsizeiintsizeiPtrVIOV glCompressedMultiTexImage1DEXTFunPtr

glCompressedMultiTexImage1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
glCompressedMultiTexImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedMultiTexImage1DEXT")

{-# NOINLINE glCompressedMultiTexImage1DEXTFunPtr #-}

-- | Usage: @'glCompressedMultiTexImage2DEXT' texunit target level internalformat width height border imageSize bits@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The length of @bits@ should be @imageSize@.


glCompressedMultiTexImage2DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
glCompressedMultiTexImage2DEXT = ffienumenumintenumsizeisizeiintsizeiPtrVIOV glCompressedMultiTexImage2DEXTFunPtr

glCompressedMultiTexImage2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
glCompressedMultiTexImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedMultiTexImage2DEXT")

{-# NOINLINE glCompressedMultiTexImage2DEXTFunPtr #-}

-- | Usage: @'glCompressedMultiTexImage3DEXT' texunit target level internalformat width height depth border imageSize bits@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The length of @bits@ should be @imageSize@.


glCompressedMultiTexImage3DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
glCompressedMultiTexImage3DEXT = ffienumenumintenumsizeisizeisizeiintsizeiPtrVIOV glCompressedMultiTexImage3DEXTFunPtr

glCompressedMultiTexImage3DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
glCompressedMultiTexImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedMultiTexImage3DEXT")

{-# NOINLINE glCompressedMultiTexImage3DEXTFunPtr #-}

-- | Usage: @'glCompressedMultiTexSubImage1DEXT' texunit target level xoffset width format imageSize bits@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The length of @bits@ should be @imageSize@.


glCompressedMultiTexSubImage1DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
glCompressedMultiTexSubImage1DEXT = ffienumenumintintsizeienumsizeiPtrVIOV glCompressedMultiTexSubImage1DEXTFunPtr

glCompressedMultiTexSubImage1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
glCompressedMultiTexSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedMultiTexSubImage1DEXT")

{-# NOINLINE glCompressedMultiTexSubImage1DEXTFunPtr #-}

-- | Usage: @'glCompressedMultiTexSubImage2DEXT' texunit target level xoffset yoffset width height format imageSize bits@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The length of @bits@ should be @imageSize@.


glCompressedMultiTexSubImage2DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
glCompressedMultiTexSubImage2DEXT = ffienumenumintintintsizeisizeienumsizeiPtrVIOV glCompressedMultiTexSubImage2DEXTFunPtr

glCompressedMultiTexSubImage2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
glCompressedMultiTexSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedMultiTexSubImage2DEXT")

{-# NOINLINE glCompressedMultiTexSubImage2DEXTFunPtr #-}

-- | Usage: @'glCompressedMultiTexSubImage3DEXT' texunit target level xoffset yoffset zoffset width height depth format imageSize bits@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The length of @bits@ should be @imageSize@.


glCompressedMultiTexSubImage3DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
glCompressedMultiTexSubImage3DEXT = ffienumenumintintintintsizeisizeisizeienumsizeiPtrVIOV glCompressedMultiTexSubImage3DEXTFunPtr

glCompressedMultiTexSubImage3DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
glCompressedMultiTexSubImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedMultiTexSubImage3DEXT")

{-# NOINLINE glCompressedMultiTexSubImage3DEXTFunPtr #-}

-- | Usage: @'glCompressedTextureImage1DEXT' texture target level internalformat width border imageSize bits@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The length of @bits@ should be @imageSize@.


glCompressedTextureImage1DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
glCompressedTextureImage1DEXT = ffiuintenumintenumsizeiintsizeiPtrVIOV glCompressedTextureImage1DEXTFunPtr

glCompressedTextureImage1DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
glCompressedTextureImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedTextureImage1DEXT")

{-# NOINLINE glCompressedTextureImage1DEXTFunPtr #-}

-- | Usage: @'glCompressedTextureImage2DEXT' texture target level internalformat width height border imageSize bits@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The length of @bits@ should be @imageSize@.


glCompressedTextureImage2DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
glCompressedTextureImage2DEXT = ffiuintenumintenumsizeisizeiintsizeiPtrVIOV glCompressedTextureImage2DEXTFunPtr

glCompressedTextureImage2DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
glCompressedTextureImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedTextureImage2DEXT")

{-# NOINLINE glCompressedTextureImage2DEXTFunPtr #-}

-- | Usage: @'glCompressedTextureImage3DEXT' texture target level internalformat width height depth border imageSize bits@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The length of @bits@ should be @imageSize@.


glCompressedTextureImage3DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
glCompressedTextureImage3DEXT = ffiuintenumintenumsizeisizeisizeiintsizeiPtrVIOV glCompressedTextureImage3DEXTFunPtr

glCompressedTextureImage3DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
glCompressedTextureImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedTextureImage3DEXT")

{-# NOINLINE glCompressedTextureImage3DEXTFunPtr #-}

-- | Usage: @'glCompressedTextureSubImage1DEXT' texture target level xoffset width format imageSize bits@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The length of @bits@ should be @imageSize@.


glCompressedTextureSubImage1DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
glCompressedTextureSubImage1DEXT = ffiuintenumintintsizeienumsizeiPtrVIOV glCompressedTextureSubImage1DEXTFunPtr

glCompressedTextureSubImage1DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
glCompressedTextureSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedTextureSubImage1DEXT")

{-# NOINLINE glCompressedTextureSubImage1DEXTFunPtr #-}

-- | Usage: @'glCompressedTextureSubImage2DEXT' texture target level xoffset yoffset width height format imageSize bits@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The length of @bits@ should be @imageSize@.


glCompressedTextureSubImage2DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
glCompressedTextureSubImage2DEXT = ffiuintenumintintintsizeisizeienumsizeiPtrVIOV glCompressedTextureSubImage2DEXTFunPtr

glCompressedTextureSubImage2DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
glCompressedTextureSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedTextureSubImage2DEXT")

{-# NOINLINE glCompressedTextureSubImage2DEXTFunPtr #-}

-- | Usage: @'glCompressedTextureSubImage3DEXT' texture target level xoffset yoffset zoffset width height depth format imageSize bits@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The length of @bits@ should be @imageSize@.


glCompressedTextureSubImage3DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
glCompressedTextureSubImage3DEXT = ffiuintenumintintintintsizeisizeisizeienumsizeiPtrVIOV glCompressedTextureSubImage3DEXTFunPtr

glCompressedTextureSubImage3DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
glCompressedTextureSubImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glCompressedTextureSubImage3DEXT")

{-# NOINLINE glCompressedTextureSubImage3DEXTFunPtr #-}

-- | Usage: @'glCopyMultiTexImage1DEXT' texunit target level internalformat x y width border@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- The parameter @border@ is a @CheckedInt32@.


glCopyMultiTexImage1DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> m ()
glCopyMultiTexImage1DEXT = ffienumenumintenumintintsizeiintIOV glCopyMultiTexImage1DEXTFunPtr

glCopyMultiTexImage1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
glCopyMultiTexImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyMultiTexImage1DEXT")

{-# NOINLINE glCopyMultiTexImage1DEXTFunPtr #-}

-- | Usage: @'glCopyMultiTexImage2DEXT' texunit target level internalformat x y width height border@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- The parameter @border@ is a @CheckedInt32@.


glCopyMultiTexImage2DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> m ()
glCopyMultiTexImage2DEXT = ffienumenumintenumintintsizeisizeiintIOV glCopyMultiTexImage2DEXTFunPtr

glCopyMultiTexImage2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
glCopyMultiTexImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyMultiTexImage2DEXT")

{-# NOINLINE glCopyMultiTexImage2DEXTFunPtr #-}

-- | Usage: @'glCopyMultiTexSubImage1DEXT' texunit target level xoffset x y width@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyMultiTexSubImage1DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> m ()
glCopyMultiTexSubImage1DEXT = ffienumenumintintintintsizeiIOV glCopyMultiTexSubImage1DEXTFunPtr

glCopyMultiTexSubImage1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
glCopyMultiTexSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyMultiTexSubImage1DEXT")

{-# NOINLINE glCopyMultiTexSubImage1DEXTFunPtr #-}

-- | Usage: @'glCopyMultiTexSubImage2DEXT' texunit target level xoffset yoffset x y width height@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyMultiTexSubImage2DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyMultiTexSubImage2DEXT = ffienumenumintintintintintsizeisizeiIOV glCopyMultiTexSubImage2DEXTFunPtr

glCopyMultiTexSubImage2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyMultiTexSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyMultiTexSubImage2DEXT")

{-# NOINLINE glCopyMultiTexSubImage2DEXTFunPtr #-}

-- | Usage: @'glCopyMultiTexSubImage3DEXT' texunit target level xoffset yoffset zoffset x y width height@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyMultiTexSubImage3DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyMultiTexSubImage3DEXT = ffienumenumintintintintintintsizeisizeiIOV glCopyMultiTexSubImage3DEXTFunPtr

glCopyMultiTexSubImage3DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyMultiTexSubImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyMultiTexSubImage3DEXT")

{-# NOINLINE glCopyMultiTexSubImage3DEXTFunPtr #-}

-- | Usage: @'glCopyTextureImage1DEXT' texture target level internalformat x y width border@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- The parameter @border@ is a @CheckedInt32@.


glCopyTextureImage1DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> m ()
glCopyTextureImage1DEXT = ffiuintenumintenumintintsizeiintIOV glCopyTextureImage1DEXTFunPtr

glCopyTextureImage1DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
glCopyTextureImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTextureImage1DEXT")

{-# NOINLINE glCopyTextureImage1DEXTFunPtr #-}

-- | Usage: @'glCopyTextureImage2DEXT' texture target level internalformat x y width height border@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- The parameter @border@ is a @CheckedInt32@.


glCopyTextureImage2DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> m ()
glCopyTextureImage2DEXT = ffiuintenumintenumintintsizeisizeiintIOV glCopyTextureImage2DEXTFunPtr

glCopyTextureImage2DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
glCopyTextureImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTextureImage2DEXT")

{-# NOINLINE glCopyTextureImage2DEXTFunPtr #-}

-- | Usage: @'glCopyTextureSubImage1DEXT' texture target level xoffset x y width@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyTextureSubImage1DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> m ()
glCopyTextureSubImage1DEXT = ffiuintenumintintintintsizeiIOV glCopyTextureSubImage1DEXTFunPtr

glCopyTextureSubImage1DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
glCopyTextureSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTextureSubImage1DEXT")

{-# NOINLINE glCopyTextureSubImage1DEXTFunPtr #-}

-- | Usage: @'glCopyTextureSubImage2DEXT' texture target level xoffset yoffset x y width height@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyTextureSubImage2DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyTextureSubImage2DEXT = ffiuintenumintintintintintsizeisizeiIOV glCopyTextureSubImage2DEXTFunPtr

glCopyTextureSubImage2DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyTextureSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTextureSubImage2DEXT")

{-# NOINLINE glCopyTextureSubImage2DEXTFunPtr #-}

-- | Usage: @'glCopyTextureSubImage3DEXT' texture target level xoffset yoffset zoffset x y width height@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.


glCopyTextureSubImage3DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyTextureSubImage3DEXT = ffiuintenumintintintintintintsizeisizeiIOV glCopyTextureSubImage3DEXTFunPtr

glCopyTextureSubImage3DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyTextureSubImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glCopyTextureSubImage3DEXT")

{-# NOINLINE glCopyTextureSubImage3DEXTFunPtr #-}

-- | Usage: @'glDisableClientStateIndexedEXT' array index@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.


glDisableClientStateIndexedEXT :: MonadIO m => GLenum -> GLuint -> m ()
glDisableClientStateIndexedEXT = ffienumuintIOV glDisableClientStateIndexedEXTFunPtr

glDisableClientStateIndexedEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glDisableClientStateIndexedEXTFunPtr = unsafePerformIO (getProcAddress "glDisableClientStateIndexedEXT")

{-# NOINLINE glDisableClientStateIndexedEXTFunPtr #-}

-- | Usage: @'glDisableClientStateiEXT' array index@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.


glDisableClientStateiEXT :: MonadIO m => GLenum -> GLuint -> m ()
glDisableClientStateiEXT = ffienumuintIOV glDisableClientStateiEXTFunPtr

glDisableClientStateiEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glDisableClientStateiEXTFunPtr = unsafePerformIO (getProcAddress "glDisableClientStateiEXT")

{-# NOINLINE glDisableClientStateiEXTFunPtr #-}

-- | Usage: @'glDisableVertexArrayAttribEXT' vaobj index@


glDisableVertexArrayAttribEXT :: MonadIO m => GLuint -> GLuint -> m ()
glDisableVertexArrayAttribEXT = ffiuintuintIOV glDisableVertexArrayAttribEXTFunPtr

glDisableVertexArrayAttribEXTFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glDisableVertexArrayAttribEXTFunPtr = unsafePerformIO (getProcAddress "glDisableVertexArrayAttribEXT")

{-# NOINLINE glDisableVertexArrayAttribEXTFunPtr #-}

-- | Usage: @'glDisableVertexArrayEXT' vaobj array@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.


glDisableVertexArrayEXT :: MonadIO m => GLuint -> GLenum -> m ()
glDisableVertexArrayEXT = ffiuintenumIOV glDisableVertexArrayEXTFunPtr

glDisableVertexArrayEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glDisableVertexArrayEXTFunPtr = unsafePerformIO (getProcAddress "glDisableVertexArrayEXT")

{-# NOINLINE glDisableVertexArrayEXTFunPtr #-}

-- | Usage: @'glEnableClientStateIndexedEXT' array index@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.


glEnableClientStateIndexedEXT :: MonadIO m => GLenum -> GLuint -> m ()
glEnableClientStateIndexedEXT = ffienumuintIOV glEnableClientStateIndexedEXTFunPtr

glEnableClientStateIndexedEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glEnableClientStateIndexedEXTFunPtr = unsafePerformIO (getProcAddress "glEnableClientStateIndexedEXT")

{-# NOINLINE glEnableClientStateIndexedEXTFunPtr #-}

-- | Usage: @'glEnableClientStateiEXT' array index@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.


glEnableClientStateiEXT :: MonadIO m => GLenum -> GLuint -> m ()
glEnableClientStateiEXT = ffienumuintIOV glEnableClientStateiEXTFunPtr

glEnableClientStateiEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glEnableClientStateiEXTFunPtr = unsafePerformIO (getProcAddress "glEnableClientStateiEXT")

{-# NOINLINE glEnableClientStateiEXTFunPtr #-}

-- | Usage: @'glEnableVertexArrayAttribEXT' vaobj index@


glEnableVertexArrayAttribEXT :: MonadIO m => GLuint -> GLuint -> m ()
glEnableVertexArrayAttribEXT = ffiuintuintIOV glEnableVertexArrayAttribEXTFunPtr

glEnableVertexArrayAttribEXTFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glEnableVertexArrayAttribEXTFunPtr = unsafePerformIO (getProcAddress "glEnableVertexArrayAttribEXT")

{-# NOINLINE glEnableVertexArrayAttribEXTFunPtr #-}

-- | Usage: @'glEnableVertexArrayEXT' vaobj array@
--
-- The parameter @array@ is a @EnableCap@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA_TEST', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_DRAW_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncHistogram.GL_ASYNC_HISTOGRAM_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_READ_PIXELS_SGIX', 'Graphics.GL.Ext.SGIX.AsyncPixel.GL_ASYNC_TEX_IMAGE_SGIX', 'Graphics.GL.Internal.Shared.GL_AUTO_NORMAL', 'Graphics.GL.Internal.Shared.GL_BLEND', 'Graphics.GL.Ext.SGIX.CalligraphicFragment.GL_CALLIGRAPHIC_FRAGMENT_SGIX', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE0', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE1', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE2', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE3', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE4', 'Graphics.GL.Internal.Shared.GL_CLIP_PLANE5', 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY', 'Graphics.GL.Internal.Shared.GL_COLOR_LOGIC_OP', 'Graphics.GL.Internal.Shared.GL_COLOR_MATERIAL', 'Graphics.GL.Ext.SGI.ColorTable.GL_COLOR_TABLE_SGI', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_1D_EXT', 'Graphics.GL.Ext.EXT.Convolution.GL_CONVOLUTION_2D_EXT', 'Graphics.GL.Internal.Shared.GL_CULL_FACE', 'Graphics.GL.Internal.Shared.GL_DEPTH_TEST', 'Graphics.GL.Internal.Shared.GL_DITHER', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY', 'Graphics.GL.Internal.Shared.GL_FOG', 'Graphics.GL.Ext.SGIX.FogOffset.GL_FOG_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_COLOR_MATERIAL_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHTING_SGIX', 'Graphics.GL.Ext.SGIX.Framezoom.GL_FRAMEZOOM_SGIX', 'Graphics.GL.Ext.EXT.Histogram.GL_HISTOGRAM_EXT', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY', 'Graphics.GL.Internal.Shared.GL_INDEX_LOGIC_OP', 'Graphics.GL.Ext.SGIX.Interlace.GL_INTERLACE_SGIX', 'Graphics.GL.Ext.SGIX.IrInstrument1.GL_IR_INSTRUMENT1_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7', 'Graphics.GL.Internal.Shared.GL_LIGHTING', 'Graphics.GL.Internal.Shared.GL_LINE_SMOOTH', 'Graphics.GL.Internal.Shared.GL_LINE_STIPPLE', 'Graphics.GL.Internal.Shared.GL_MAP1_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP1_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP1_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP1_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP1_VERTEX_4', 'Graphics.GL.Internal.Shared.GL_MAP2_COLOR_4', 'Graphics.GL.Internal.Shared.GL_MAP2_INDEX', 'Graphics.GL.Internal.Shared.GL_MAP2_NORMAL', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_1', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_2', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_3', 'Graphics.GL.Internal.Shared.GL_MAP2_TEXTURE_COORD_4', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_3', 'Graphics.GL.Internal.Shared.GL_MAP2_VERTEX_4', 'Graphics.GL.Ext.EXT.Histogram.GL_MINMAX_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_MULTISAMPLE_SGIS', 'Graphics.GL.Internal.Shared.GL_NORMALIZE', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY', 'Graphics.GL.Ext.SGIS.PixelTexture.GL_PIXEL_TEXTURE_SGIS', 'Graphics.GL.Ext.SGIX.PixelTexture.GL_PIXEL_TEX_GEN_SGIX', 'Graphics.GL.Internal.Shared.GL_POINT_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_FILL', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_LINE', 'Graphics.GL.Internal.Shared.GL_POLYGON_OFFSET_POINT', 'Graphics.GL.Internal.Shared.GL_POLYGON_SMOOTH', 'Graphics.GL.Internal.Shared.GL_POLYGON_STIPPLE', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGI.ColorTable.GL_POST_CONVOLUTION_COLOR_TABLE_SGI', 'Graphics.GL.Ext.SGIX.ReferencePlane.GL_REFERENCE_PLANE_SGIX', 'Graphics.GL.Ext.EXT.RescaleNormal.GL_RESCALE_NORMAL_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_MASK_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_ALPHA_TO_ONE_SGIS', 'Graphics.GL.Ext.SGIS.Multisample.GL_SAMPLE_MASK_SGIS', 'Graphics.GL.Internal.Shared.GL_SCISSOR_TEST', 'Graphics.GL.Ext.EXT.Convolution.GL_SEPARABLE_2D_EXT', 'Graphics.GL.Ext.EXT.SharedTexturePalette.GL_SHARED_TEXTURE_PALETTE_EXT', 'Graphics.GL.Ext.SGIX.Sprite.GL_SPRITE_SGIX', 'Graphics.GL.Internal.Shared.GL_STENCIL_TEST', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Ext.SGI.TextureColorTable.GL_TEXTURE_COLOR_TABLE_SGI', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_Q', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_R', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_T', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY'.


glEnableVertexArrayEXT :: MonadIO m => GLuint -> GLenum -> m ()
glEnableVertexArrayEXT = ffiuintenumIOV glEnableVertexArrayEXTFunPtr

glEnableVertexArrayEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glEnableVertexArrayEXTFunPtr = unsafePerformIO (getProcAddress "glEnableVertexArrayEXT")

{-# NOINLINE glEnableVertexArrayEXTFunPtr #-}

-- | Usage: @'glFlushMappedNamedBufferRangeEXT' buffer offset length@


glFlushMappedNamedBufferRangeEXT :: MonadIO m => GLuint -> GLintptr -> GLsizeiptr -> m ()
glFlushMappedNamedBufferRangeEXT = ffiuintintptrsizeiptrIOV glFlushMappedNamedBufferRangeEXTFunPtr

glFlushMappedNamedBufferRangeEXTFunPtr :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ())
glFlushMappedNamedBufferRangeEXTFunPtr = unsafePerformIO (getProcAddress "glFlushMappedNamedBufferRangeEXT")

{-# NOINLINE glFlushMappedNamedBufferRangeEXTFunPtr #-}

-- | Usage: @'glFramebufferDrawBufferEXT' framebuffer mode@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @mode@ is a @DrawBufferMode@, one of: 'Graphics.GL.Internal.Shared.GL_AUX0', 'Graphics.GL.Internal.Shared.GL_AUX1', 'Graphics.GL.Internal.Shared.GL_AUX2', 'Graphics.GL.Internal.Shared.GL_AUX3', 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_BACK_LEFT', 'Graphics.GL.Internal.Shared.GL_BACK_RIGHT', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT_LEFT', 'Graphics.GL.Internal.Shared.GL_FRONT_RIGHT', 'Graphics.GL.Internal.Shared.GL_LEFT', 'Graphics.GL.Internal.Shared.GL_NONE', 'Graphics.GL.Ext.OES.FramebufferObject.GL_NONE_OES', 'Graphics.GL.Internal.Shared.GL_RIGHT'.


glFramebufferDrawBufferEXT :: MonadIO m => GLuint -> GLenum -> m ()
glFramebufferDrawBufferEXT = ffiuintenumIOV glFramebufferDrawBufferEXTFunPtr

glFramebufferDrawBufferEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glFramebufferDrawBufferEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferDrawBufferEXT")

{-# NOINLINE glFramebufferDrawBufferEXTFunPtr #-}

-- | Usage: @'glFramebufferDrawBuffersEXT' framebuffer n bufs@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @bufs@ is a @DrawBufferMode@, one of: 'Graphics.GL.Internal.Shared.GL_AUX0', 'Graphics.GL.Internal.Shared.GL_AUX1', 'Graphics.GL.Internal.Shared.GL_AUX2', 'Graphics.GL.Internal.Shared.GL_AUX3', 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_BACK_LEFT', 'Graphics.GL.Internal.Shared.GL_BACK_RIGHT', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT_LEFT', 'Graphics.GL.Internal.Shared.GL_FRONT_RIGHT', 'Graphics.GL.Internal.Shared.GL_LEFT', 'Graphics.GL.Internal.Shared.GL_NONE', 'Graphics.GL.Ext.OES.FramebufferObject.GL_NONE_OES', 'Graphics.GL.Internal.Shared.GL_RIGHT'.
--
-- The length of @bufs@ should be @n@.


glFramebufferDrawBuffersEXT :: MonadIO m => GLuint -> GLsizei -> Ptr GLenum -> m ()
glFramebufferDrawBuffersEXT = ffiuintsizeiPtrenumIOV glFramebufferDrawBuffersEXTFunPtr

glFramebufferDrawBuffersEXTFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ())
glFramebufferDrawBuffersEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferDrawBuffersEXT")

{-# NOINLINE glFramebufferDrawBuffersEXTFunPtr #-}

-- | Usage: @'glFramebufferReadBufferEXT' framebuffer mode@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @mode@ is a @ReadBufferMode@, one of: 'Graphics.GL.Internal.Shared.GL_AUX0', 'Graphics.GL.Internal.Shared.GL_AUX1', 'Graphics.GL.Internal.Shared.GL_AUX2', 'Graphics.GL.Internal.Shared.GL_AUX3', 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_BACK_LEFT', 'Graphics.GL.Internal.Shared.GL_BACK_RIGHT', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_LEFT', 'Graphics.GL.Internal.Shared.GL_FRONT_RIGHT', 'Graphics.GL.Internal.Shared.GL_LEFT', 'Graphics.GL.Internal.Shared.GL_RIGHT'.


glFramebufferReadBufferEXT :: MonadIO m => GLuint -> GLenum -> m ()
glFramebufferReadBufferEXT = ffiuintenumIOV glFramebufferReadBufferEXTFunPtr

glFramebufferReadBufferEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glFramebufferReadBufferEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferReadBufferEXT")

{-# NOINLINE glFramebufferReadBufferEXTFunPtr #-}

-- | Usage: @'glGenerateMultiTexMipmapEXT' texunit target@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.


glGenerateMultiTexMipmapEXT :: MonadIO m => GLenum -> GLenum -> m ()
glGenerateMultiTexMipmapEXT = ffienumenumIOV glGenerateMultiTexMipmapEXTFunPtr

glGenerateMultiTexMipmapEXTFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glGenerateMultiTexMipmapEXTFunPtr = unsafePerformIO (getProcAddress "glGenerateMultiTexMipmapEXT")

{-# NOINLINE glGenerateMultiTexMipmapEXTFunPtr #-}

-- | Usage: @'glGenerateTextureMipmapEXT' texture target@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.


glGenerateTextureMipmapEXT :: MonadIO m => GLuint -> GLenum -> m ()
glGenerateTextureMipmapEXT = ffiuintenumIOV glGenerateTextureMipmapEXTFunPtr

glGenerateTextureMipmapEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glGenerateTextureMipmapEXTFunPtr = unsafePerformIO (getProcAddress "glGenerateTextureMipmapEXT")

{-# NOINLINE glGenerateTextureMipmapEXTFunPtr #-}

-- | Usage: @'glGetCompressedMultiTexImageEXT' texunit target lod img@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @lod@ is a @CheckedInt32@.
--
-- The length of @img@ should be @COMPSIZE(target,lod)@.


glGetCompressedMultiTexImageEXT :: MonadIO m => GLenum -> GLenum -> GLint -> Ptr () -> m ()
glGetCompressedMultiTexImageEXT = ffienumenumintPtrVIOV glGetCompressedMultiTexImageEXTFunPtr

glGetCompressedMultiTexImageEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> Ptr () -> IO ())
glGetCompressedMultiTexImageEXTFunPtr = unsafePerformIO (getProcAddress "glGetCompressedMultiTexImageEXT")

{-# NOINLINE glGetCompressedMultiTexImageEXTFunPtr #-}

-- | Usage: @'glGetCompressedTextureImageEXT' texture target lod img@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @lod@ is a @CheckedInt32@.
--
-- The length of @img@ should be @COMPSIZE(target,lod)@.


glGetCompressedTextureImageEXT :: MonadIO m => GLuint -> GLenum -> GLint -> Ptr () -> m ()
glGetCompressedTextureImageEXT = ffiuintenumintPtrVIOV glGetCompressedTextureImageEXTFunPtr

glGetCompressedTextureImageEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> Ptr () -> IO ())
glGetCompressedTextureImageEXTFunPtr = unsafePerformIO (getProcAddress "glGetCompressedTextureImageEXT")

{-# NOINLINE glGetCompressedTextureImageEXTFunPtr #-}

-- | Usage: @'glGetDoubleIndexedvEXT' target index data@
--
-- The parameter @target@ is a @TypeEnum@.
--
-- The length of @data@ should be @COMPSIZE(target)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetDoublei_v'.


glGetDoubleIndexedvEXT :: MonadIO m => GLenum -> GLuint -> Ptr GLdouble -> m ()
glGetDoubleIndexedvEXT = ffienumuintPtrdoubleIOV glGetDoubleIndexedvEXTFunPtr

glGetDoubleIndexedvEXTFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
glGetDoubleIndexedvEXTFunPtr = unsafePerformIO (getProcAddress "glGetDoubleIndexedvEXT")

{-# NOINLINE glGetDoubleIndexedvEXTFunPtr #-}

-- | Usage: @'glGetDoublei_vEXT' pname index params@
--
-- The parameter @pname@ is a @TypeEnum@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetDoublei_v'.


glGetDoublei_vEXT :: MonadIO m => GLenum -> GLuint -> Ptr GLdouble -> m ()
glGetDoublei_vEXT = ffienumuintPtrdoubleIOV glGetDoublei_vEXTFunPtr

glGetDoublei_vEXTFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
glGetDoublei_vEXTFunPtr = unsafePerformIO (getProcAddress "glGetDoublei_vEXT")

{-# NOINLINE glGetDoublei_vEXTFunPtr #-}

-- | Usage: @'glGetFloatIndexedvEXT' target index data@
--
-- The parameter @target@ is a @TypeEnum@.
--
-- The length of @data@ should be @COMPSIZE(target)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetFloati_v'.


glGetFloatIndexedvEXT :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glGetFloatIndexedvEXT = ffienumuintPtrfloatIOV glGetFloatIndexedvEXTFunPtr

glGetFloatIndexedvEXTFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glGetFloatIndexedvEXTFunPtr = unsafePerformIO (getProcAddress "glGetFloatIndexedvEXT")

{-# NOINLINE glGetFloatIndexedvEXTFunPtr #-}

-- | Usage: @'glGetFloati_vEXT' pname index params@
--
-- The parameter @pname@ is a @TypeEnum@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetFloati_v'.


glGetFloati_vEXT :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glGetFloati_vEXT = ffienumuintPtrfloatIOV glGetFloati_vEXTFunPtr

glGetFloati_vEXTFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glGetFloati_vEXTFunPtr = unsafePerformIO (getProcAddress "glGetFloati_vEXT")

{-# NOINLINE glGetFloati_vEXTFunPtr #-}

-- | Usage: @'glGetFramebufferParameterivEXT' framebuffer pname params@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @pname@ is a @GetFramebufferParameter@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFramebufferParameterivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetFramebufferParameterivEXT = ffiuintenumPtrintIOV glGetFramebufferParameterivEXTFunPtr

glGetFramebufferParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetFramebufferParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetFramebufferParameterivEXT")

{-# NOINLINE glGetFramebufferParameterivEXTFunPtr #-}

-- | Usage: @'glGetMultiTexEnvfvEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureEnvTarget@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV'.
--
-- The parameter @pname@ is a @TextureEnvParameter@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_COLOR', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_MODE'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexEnvfvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetMultiTexEnvfvEXT = ffienumenumenumPtrfloatIOV glGetMultiTexEnvfvEXTFunPtr

glGetMultiTexEnvfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetMultiTexEnvfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexEnvfvEXT")

{-# NOINLINE glGetMultiTexEnvfvEXTFunPtr #-}

-- | Usage: @'glGetMultiTexEnvivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureEnvTarget@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV'.
--
-- The parameter @pname@ is a @TextureEnvParameter@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_COLOR', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_MODE'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexEnvivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetMultiTexEnvivEXT = ffienumenumenumPtrintIOV glGetMultiTexEnvivEXTFunPtr

glGetMultiTexEnvivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetMultiTexEnvivEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexEnvivEXT")

{-# NOINLINE glGetMultiTexEnvivEXTFunPtr #-}

-- | Usage: @'glGetMultiTexGendvEXT' texunit coord pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexGendvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLdouble -> m ()
glGetMultiTexGendvEXT = ffienumenumenumPtrdoubleIOV glGetMultiTexGendvEXTFunPtr

glGetMultiTexGendvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ())
glGetMultiTexGendvEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexGendvEXT")

{-# NOINLINE glGetMultiTexGendvEXTFunPtr #-}

-- | Usage: @'glGetMultiTexGenfvEXT' texunit coord pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexGenfvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetMultiTexGenfvEXT = ffienumenumenumPtrfloatIOV glGetMultiTexGenfvEXTFunPtr

glGetMultiTexGenfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetMultiTexGenfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexGenfvEXT")

{-# NOINLINE glGetMultiTexGenfvEXTFunPtr #-}

-- | Usage: @'glGetMultiTexGenivEXT' texunit coord pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexGenivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetMultiTexGenivEXT = ffienumenumenumPtrintIOV glGetMultiTexGenivEXTFunPtr

glGetMultiTexGenivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetMultiTexGenivEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexGenivEXT")

{-# NOINLINE glGetMultiTexGenivEXTFunPtr #-}

-- | Usage: @'glGetMultiTexImageEXT' texunit target level format type pixels@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(target,level,format,type)@.


glGetMultiTexImageEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glGetMultiTexImageEXT = ffienumenumintenumenumPtrVIOV glGetMultiTexImageEXTFunPtr

glGetMultiTexImageEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glGetMultiTexImageEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexImageEXT")

{-# NOINLINE glGetMultiTexImageEXTFunPtr #-}

-- | Usage: @'glGetMultiTexLevelParameterfvEXT' texunit target level pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexLevelParameterfvEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> m ()
glGetMultiTexLevelParameterfvEXT = ffienumenumintenumPtrfloatIOV glGetMultiTexLevelParameterfvEXTFunPtr

glGetMultiTexLevelParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
glGetMultiTexLevelParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexLevelParameterfvEXT")

{-# NOINLINE glGetMultiTexLevelParameterfvEXTFunPtr #-}

-- | Usage: @'glGetMultiTexLevelParameterivEXT' texunit target level pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexLevelParameterivEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> m ()
glGetMultiTexLevelParameterivEXT = ffienumenumintenumPtrintIOV glGetMultiTexLevelParameterivEXTFunPtr

glGetMultiTexLevelParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
glGetMultiTexLevelParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexLevelParameterivEXT")

{-# NOINLINE glGetMultiTexLevelParameterivEXTFunPtr #-}

-- | Usage: @'glGetMultiTexParameterIivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexParameterIivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetMultiTexParameterIivEXT = ffienumenumenumPtrintIOV glGetMultiTexParameterIivEXTFunPtr

glGetMultiTexParameterIivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetMultiTexParameterIivEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexParameterIivEXT")

{-# NOINLINE glGetMultiTexParameterIivEXTFunPtr #-}

-- | Usage: @'glGetMultiTexParameterIuivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexParameterIuivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLuint -> m ()
glGetMultiTexParameterIuivEXT = ffienumenumenumPtruintIOV glGetMultiTexParameterIuivEXTFunPtr

glGetMultiTexParameterIuivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ())
glGetMultiTexParameterIuivEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexParameterIuivEXT")

{-# NOINLINE glGetMultiTexParameterIuivEXTFunPtr #-}

-- | Usage: @'glGetMultiTexParameterfvEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexParameterfvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetMultiTexParameterfvEXT = ffienumenumenumPtrfloatIOV glGetMultiTexParameterfvEXTFunPtr

glGetMultiTexParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetMultiTexParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexParameterfvEXT")

{-# NOINLINE glGetMultiTexParameterfvEXTFunPtr #-}

-- | Usage: @'glGetMultiTexParameterivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetMultiTexParameterivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetMultiTexParameterivEXT = ffienumenumenumPtrintIOV glGetMultiTexParameterivEXTFunPtr

glGetMultiTexParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetMultiTexParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetMultiTexParameterivEXT")

{-# NOINLINE glGetMultiTexParameterivEXTFunPtr #-}

-- | Usage: @'glGetNamedBufferParameterivEXT' buffer pname params@
--
-- The parameter @pname@ is a @VertexBufferObjectParameter@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetNamedBufferParameterivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetNamedBufferParameterivEXT = ffiuintenumPtrintIOV glGetNamedBufferParameterivEXTFunPtr

glGetNamedBufferParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetNamedBufferParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedBufferParameterivEXT")

{-# NOINLINE glGetNamedBufferParameterivEXTFunPtr #-}

-- | Usage: @'glGetNamedBufferPointervEXT' buffer pname params@
--
-- The parameter @pname@ is a @VertexBufferObjectParameter@.
--
-- The length of @params@ should be @1@.


glGetNamedBufferPointervEXT :: MonadIO m => GLuint -> GLenum -> Ptr (Ptr ()) -> m ()
glGetNamedBufferPointervEXT = ffiuintenumPtrPtrVIOV glGetNamedBufferPointervEXTFunPtr

glGetNamedBufferPointervEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetNamedBufferPointervEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedBufferPointervEXT")

{-# NOINLINE glGetNamedBufferPointervEXTFunPtr #-}

-- | Usage: @'glGetNamedBufferSubDataEXT' buffer offset size data@
--
-- The length of @data@ should be @COMPSIZE(size)@.


glGetNamedBufferSubDataEXT :: MonadIO m => GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
glGetNamedBufferSubDataEXT = ffiuintintptrsizeiptrPtrVIOV glGetNamedBufferSubDataEXTFunPtr

glGetNamedBufferSubDataEXTFunPtr :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> IO ())
glGetNamedBufferSubDataEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedBufferSubDataEXT")

{-# NOINLINE glGetNamedBufferSubDataEXTFunPtr #-}

-- | Usage: @'glGetNamedFramebufferAttachmentParameterivEXT' framebuffer attachment pname params@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @pname@ is a @FramebufferAttachmentParameterName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetNamedFramebufferAttachmentParameterivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetNamedFramebufferAttachmentParameterivEXT = ffiuintenumenumPtrintIOV glGetNamedFramebufferAttachmentParameterivEXTFunPtr

glGetNamedFramebufferAttachmentParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetNamedFramebufferAttachmentParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedFramebufferAttachmentParameterivEXT")

{-# NOINLINE glGetNamedFramebufferAttachmentParameterivEXTFunPtr #-}

-- | Usage: @'glGetNamedFramebufferParameterivEXT' framebuffer pname params@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @pname@ is a @GetFramebufferParameter@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetNamedFramebufferParameterivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetNamedFramebufferParameterivEXT = ffiuintenumPtrintIOV glGetNamedFramebufferParameterivEXTFunPtr

glGetNamedFramebufferParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetNamedFramebufferParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedFramebufferParameterivEXT")

{-# NOINLINE glGetNamedFramebufferParameterivEXTFunPtr #-}

-- | Usage: @'glGetNamedProgramLocalParameterIivEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetNamedProgramLocalParameterIivEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLint -> m ()
glGetNamedProgramLocalParameterIivEXT = ffiuintenumuintPtrintIOV glGetNamedProgramLocalParameterIivEXTFunPtr

glGetNamedProgramLocalParameterIivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
glGetNamedProgramLocalParameterIivEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedProgramLocalParameterIivEXT")

{-# NOINLINE glGetNamedProgramLocalParameterIivEXTFunPtr #-}

-- | Usage: @'glGetNamedProgramLocalParameterIuivEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetNamedProgramLocalParameterIuivEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLuint -> m ()
glGetNamedProgramLocalParameterIuivEXT = ffiuintenumuintPtruintIOV glGetNamedProgramLocalParameterIuivEXTFunPtr

glGetNamedProgramLocalParameterIuivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ())
glGetNamedProgramLocalParameterIuivEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedProgramLocalParameterIuivEXT")

{-# NOINLINE glGetNamedProgramLocalParameterIuivEXTFunPtr #-}

-- | Usage: @'glGetNamedProgramLocalParameterdvEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetNamedProgramLocalParameterdvEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLdouble -> m ()
glGetNamedProgramLocalParameterdvEXT = ffiuintenumuintPtrdoubleIOV glGetNamedProgramLocalParameterdvEXTFunPtr

glGetNamedProgramLocalParameterdvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ())
glGetNamedProgramLocalParameterdvEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedProgramLocalParameterdvEXT")

{-# NOINLINE glGetNamedProgramLocalParameterdvEXTFunPtr #-}

-- | Usage: @'glGetNamedProgramLocalParameterfvEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetNamedProgramLocalParameterfvEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLfloat -> m ()
glGetNamedProgramLocalParameterfvEXT = ffiuintenumuintPtrfloatIOV glGetNamedProgramLocalParameterfvEXTFunPtr

glGetNamedProgramLocalParameterfvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ())
glGetNamedProgramLocalParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedProgramLocalParameterfvEXT")

{-# NOINLINE glGetNamedProgramLocalParameterfvEXTFunPtr #-}

-- | Usage: @'glGetNamedProgramStringEXT' program target pname string@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The parameter @pname@ is a @ProgramStringProperty@.
--
-- The length of @string@ should be @COMPSIZE(program,pname)@.


glGetNamedProgramStringEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr () -> m ()
glGetNamedProgramStringEXT = ffiuintenumenumPtrVIOV glGetNamedProgramStringEXTFunPtr

glGetNamedProgramStringEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr () -> IO ())
glGetNamedProgramStringEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedProgramStringEXT")

{-# NOINLINE glGetNamedProgramStringEXTFunPtr #-}

-- | Usage: @'glGetNamedProgramivEXT' program target pname params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The parameter @pname@ is a @ProgramProperty@.
--
-- The length of @params@ should be @1@.


glGetNamedProgramivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetNamedProgramivEXT = ffiuintenumenumPtrintIOV glGetNamedProgramivEXTFunPtr

glGetNamedProgramivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetNamedProgramivEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedProgramivEXT")

{-# NOINLINE glGetNamedProgramivEXTFunPtr #-}

-- | Usage: @'glGetNamedRenderbufferParameterivEXT' renderbuffer pname params@
--
-- The parameter @renderbuffer@ is a @Renderbuffer@.
--
-- The parameter @pname@ is a @RenderbufferParameterName@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetNamedRenderbufferParameterivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetNamedRenderbufferParameterivEXT = ffiuintenumPtrintIOV glGetNamedRenderbufferParameterivEXTFunPtr

glGetNamedRenderbufferParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetNamedRenderbufferParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetNamedRenderbufferParameterivEXT")

{-# NOINLINE glGetNamedRenderbufferParameterivEXTFunPtr #-}

-- | Usage: @'glGetPointerIndexedvEXT' target index data@
--
-- The parameter @target@ is a @TypeEnum@.
--
-- The length of @data@ should be @1@.


glGetPointerIndexedvEXT :: MonadIO m => GLenum -> GLuint -> Ptr (Ptr ()) -> m ()
glGetPointerIndexedvEXT = ffienumuintPtrPtrVIOV glGetPointerIndexedvEXTFunPtr

glGetPointerIndexedvEXTFunPtr :: FunPtr (GLenum -> GLuint -> Ptr (Ptr ()) -> IO ())
glGetPointerIndexedvEXTFunPtr = unsafePerformIO (getProcAddress "glGetPointerIndexedvEXT")

{-# NOINLINE glGetPointerIndexedvEXTFunPtr #-}

-- | Usage: @'glGetPointeri_vEXT' pname index params@
--
-- The parameter @pname@ is a @TypeEnum@.
--
-- The length of @params@ should be @1@.


glGetPointeri_vEXT :: MonadIO m => GLenum -> GLuint -> Ptr (Ptr ()) -> m ()
glGetPointeri_vEXT = ffienumuintPtrPtrVIOV glGetPointeri_vEXTFunPtr

glGetPointeri_vEXTFunPtr :: FunPtr (GLenum -> GLuint -> Ptr (Ptr ()) -> IO ())
glGetPointeri_vEXTFunPtr = unsafePerformIO (getProcAddress "glGetPointeri_vEXT")

{-# NOINLINE glGetPointeri_vEXTFunPtr #-}

-- | Usage: @'glGetTextureImageEXT' texture target level format type pixels@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(target,level,format,type)@.


glGetTextureImageEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glGetTextureImageEXT = ffiuintenumintenumenumPtrVIOV glGetTextureImageEXTFunPtr

glGetTextureImageEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glGetTextureImageEXTFunPtr = unsafePerformIO (getProcAddress "glGetTextureImageEXT")

{-# NOINLINE glGetTextureImageEXTFunPtr #-}

-- | Usage: @'glGetTextureLevelParameterfvEXT' texture target level pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTextureLevelParameterfvEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> m ()
glGetTextureLevelParameterfvEXT = ffiuintenumintenumPtrfloatIOV glGetTextureLevelParameterfvEXTFunPtr

glGetTextureLevelParameterfvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
glGetTextureLevelParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetTextureLevelParameterfvEXT")

{-# NOINLINE glGetTextureLevelParameterfvEXTFunPtr #-}

-- | Usage: @'glGetTextureLevelParameterivEXT' texture target level pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTextureLevelParameterivEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> m ()
glGetTextureLevelParameterivEXT = ffiuintenumintenumPtrintIOV glGetTextureLevelParameterivEXTFunPtr

glGetTextureLevelParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
glGetTextureLevelParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetTextureLevelParameterivEXT")

{-# NOINLINE glGetTextureLevelParameterivEXTFunPtr #-}

-- | Usage: @'glGetTextureParameterIivEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTextureParameterIivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetTextureParameterIivEXT = ffiuintenumenumPtrintIOV glGetTextureParameterIivEXTFunPtr

glGetTextureParameterIivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetTextureParameterIivEXTFunPtr = unsafePerformIO (getProcAddress "glGetTextureParameterIivEXT")

{-# NOINLINE glGetTextureParameterIivEXTFunPtr #-}

-- | Usage: @'glGetTextureParameterIuivEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTextureParameterIuivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLuint -> m ()
glGetTextureParameterIuivEXT = ffiuintenumenumPtruintIOV glGetTextureParameterIuivEXTFunPtr

glGetTextureParameterIuivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ())
glGetTextureParameterIuivEXTFunPtr = unsafePerformIO (getProcAddress "glGetTextureParameterIuivEXT")

{-# NOINLINE glGetTextureParameterIuivEXTFunPtr #-}

-- | Usage: @'glGetTextureParameterfvEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTextureParameterfvEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetTextureParameterfvEXT = ffiuintenumenumPtrfloatIOV glGetTextureParameterfvEXTFunPtr

glGetTextureParameterfvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetTextureParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetTextureParameterfvEXT")

{-# NOINLINE glGetTextureParameterfvEXTFunPtr #-}

-- | Usage: @'glGetTextureParameterivEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @GetTextureParameter@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIS.SharpenTexture.GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4DSIZE_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ALPHA_SIZE', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BLUE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.NV.TextureBorderClamp.GL_TEXTURE_BORDER_COLOR_NV', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_OPERATOR_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COMPONENTS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_DEPTH_EXT', 'Graphics.GL.Ext.SGIS.TextureFilter4.GL_TEXTURE_FILTER4_SIZE_SGIS', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_GEQUAL_R_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GREEN_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_HEIGHT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTENSITY_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_INTERNAL_FORMAT', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_LEQUAL_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_LUMINANCE_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RED_SIZE', 'Graphics.GL.Internal.Shared.GL_TEXTURE_RESIDENT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WIDTH', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetTextureParameterivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetTextureParameterivEXT = ffiuintenumenumPtrintIOV glGetTextureParameterivEXTFunPtr

glGetTextureParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetTextureParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetTextureParameterivEXT")

{-# NOINLINE glGetTextureParameterivEXTFunPtr #-}

-- | Usage: @'glGetVertexArrayIntegeri_vEXT' vaobj index pname param@


glGetVertexArrayIntegeri_vEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr GLint -> m ()
glGetVertexArrayIntegeri_vEXT = ffiuintuintenumPtrintIOV glGetVertexArrayIntegeri_vEXTFunPtr

glGetVertexArrayIntegeri_vEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVertexArrayIntegeri_vEXTFunPtr = unsafePerformIO (getProcAddress "glGetVertexArrayIntegeri_vEXT")

{-# NOINLINE glGetVertexArrayIntegeri_vEXTFunPtr #-}

-- | Usage: @'glGetVertexArrayIntegervEXT' vaobj pname param@


glGetVertexArrayIntegervEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVertexArrayIntegervEXT = ffiuintenumPtrintIOV glGetVertexArrayIntegervEXTFunPtr

glGetVertexArrayIntegervEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVertexArrayIntegervEXTFunPtr = unsafePerformIO (getProcAddress "glGetVertexArrayIntegervEXT")

{-# NOINLINE glGetVertexArrayIntegervEXTFunPtr #-}

-- | Usage: @'glGetVertexArrayPointeri_vEXT' vaobj index pname param@


glGetVertexArrayPointeri_vEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr (Ptr ()) -> m ()
glGetVertexArrayPointeri_vEXT = ffiuintuintenumPtrPtrVIOV glGetVertexArrayPointeri_vEXTFunPtr

glGetVertexArrayPointeri_vEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetVertexArrayPointeri_vEXTFunPtr = unsafePerformIO (getProcAddress "glGetVertexArrayPointeri_vEXT")

{-# NOINLINE glGetVertexArrayPointeri_vEXTFunPtr #-}

-- | Usage: @'glGetVertexArrayPointervEXT' vaobj pname param@
--
-- The length of @param@ should be @1@.


glGetVertexArrayPointervEXT :: MonadIO m => GLuint -> GLenum -> Ptr (Ptr ()) -> m ()
glGetVertexArrayPointervEXT = ffiuintenumPtrPtrVIOV glGetVertexArrayPointervEXTFunPtr

glGetVertexArrayPointervEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetVertexArrayPointervEXTFunPtr = unsafePerformIO (getProcAddress "glGetVertexArrayPointervEXT")

{-# NOINLINE glGetVertexArrayPointervEXTFunPtr #-}

-- | Usage: @'glMapNamedBufferEXT' buffer access@
--
-- The parameter @access@ is a @VertexBufferObjectAccess@.


glMapNamedBufferEXT :: MonadIO m => GLuint -> GLenum -> m (Ptr ())
glMapNamedBufferEXT = ffiuintenumIOPtrV glMapNamedBufferEXTFunPtr

glMapNamedBufferEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO (Ptr ()))
glMapNamedBufferEXTFunPtr = unsafePerformIO (getProcAddress "glMapNamedBufferEXT")

{-# NOINLINE glMapNamedBufferEXTFunPtr #-}

-- | Usage: @'glMapNamedBufferRangeEXT' buffer offset length access@
--
-- The parameter @access@ is a @BufferAccessMask@.


glMapNamedBufferRangeEXT :: MonadIO m => GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> m (Ptr ())
glMapNamedBufferRangeEXT = ffiuintintptrsizeiptrbitfieldIOPtrV glMapNamedBufferRangeEXTFunPtr

glMapNamedBufferRangeEXTFunPtr :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ()))
glMapNamedBufferRangeEXTFunPtr = unsafePerformIO (getProcAddress "glMapNamedBufferRangeEXT")

{-# NOINLINE glMapNamedBufferRangeEXTFunPtr #-}

-- | Usage: @'glMatrixFrustumEXT' mode left right bottom top zNear zFar@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixFrustumEXT :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glMatrixFrustumEXT = ffienumdoubledoubledoubledoubledoubledoubleIOV glMatrixFrustumEXTFunPtr

glMatrixFrustumEXTFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glMatrixFrustumEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixFrustumEXT")

{-# NOINLINE glMatrixFrustumEXTFunPtr #-}

-- | Usage: @'glMatrixLoadIdentityEXT' mode@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixLoadIdentityEXT :: MonadIO m => GLenum -> m ()
glMatrixLoadIdentityEXT = ffienumIOV glMatrixLoadIdentityEXTFunPtr

glMatrixLoadIdentityEXTFunPtr :: FunPtr (GLenum -> IO ())
glMatrixLoadIdentityEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixLoadIdentityEXT")

{-# NOINLINE glMatrixLoadIdentityEXTFunPtr #-}

-- | Usage: @'glMatrixLoadTransposedEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixLoadTransposedEXT :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMatrixLoadTransposedEXT = ffienumPtrdoubleIOV glMatrixLoadTransposedEXTFunPtr

glMatrixLoadTransposedEXTFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMatrixLoadTransposedEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixLoadTransposedEXT")

{-# NOINLINE glMatrixLoadTransposedEXTFunPtr #-}

-- | Usage: @'glMatrixLoadTransposefEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixLoadTransposefEXT :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixLoadTransposefEXT = ffienumPtrfloatIOV glMatrixLoadTransposefEXTFunPtr

glMatrixLoadTransposefEXTFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixLoadTransposefEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixLoadTransposefEXT")

{-# NOINLINE glMatrixLoadTransposefEXTFunPtr #-}

-- | Usage: @'glMatrixLoaddEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixLoaddEXT :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMatrixLoaddEXT = ffienumPtrdoubleIOV glMatrixLoaddEXTFunPtr

glMatrixLoaddEXTFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMatrixLoaddEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixLoaddEXT")

{-# NOINLINE glMatrixLoaddEXTFunPtr #-}

-- | Usage: @'glMatrixLoadfEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixLoadfEXT :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixLoadfEXT = ffienumPtrfloatIOV glMatrixLoadfEXTFunPtr

glMatrixLoadfEXTFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixLoadfEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixLoadfEXT")

{-# NOINLINE glMatrixLoadfEXTFunPtr #-}

-- | Usage: @'glMatrixMultTransposedEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixMultTransposedEXT :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMatrixMultTransposedEXT = ffienumPtrdoubleIOV glMatrixMultTransposedEXTFunPtr

glMatrixMultTransposedEXTFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMatrixMultTransposedEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixMultTransposedEXT")

{-# NOINLINE glMatrixMultTransposedEXTFunPtr #-}

-- | Usage: @'glMatrixMultTransposefEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixMultTransposefEXT :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixMultTransposefEXT = ffienumPtrfloatIOV glMatrixMultTransposefEXTFunPtr

glMatrixMultTransposefEXTFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixMultTransposefEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixMultTransposefEXT")

{-# NOINLINE glMatrixMultTransposefEXTFunPtr #-}

-- | Usage: @'glMatrixMultdEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixMultdEXT :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMatrixMultdEXT = ffienumPtrdoubleIOV glMatrixMultdEXTFunPtr

glMatrixMultdEXTFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMatrixMultdEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixMultdEXT")

{-# NOINLINE glMatrixMultdEXTFunPtr #-}

-- | Usage: @'glMatrixMultfEXT' mode m@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.
--
-- The length of @m@ should be @16@.


glMatrixMultfEXT :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMatrixMultfEXT = ffienumPtrfloatIOV glMatrixMultfEXTFunPtr

glMatrixMultfEXTFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMatrixMultfEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixMultfEXT")

{-# NOINLINE glMatrixMultfEXTFunPtr #-}

-- | Usage: @'glMatrixOrthoEXT' mode left right bottom top zNear zFar@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixOrthoEXT :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glMatrixOrthoEXT = ffienumdoubledoubledoubledoubledoubledoubleIOV glMatrixOrthoEXTFunPtr

glMatrixOrthoEXTFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glMatrixOrthoEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixOrthoEXT")

{-# NOINLINE glMatrixOrthoEXTFunPtr #-}

-- | Usage: @'glMatrixPopEXT' mode@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixPopEXT :: MonadIO m => GLenum -> m ()
glMatrixPopEXT = ffienumIOV glMatrixPopEXTFunPtr

glMatrixPopEXTFunPtr :: FunPtr (GLenum -> IO ())
glMatrixPopEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixPopEXT")

{-# NOINLINE glMatrixPopEXTFunPtr #-}

-- | Usage: @'glMatrixPushEXT' mode@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixPushEXT :: MonadIO m => GLenum -> m ()
glMatrixPushEXT = ffienumIOV glMatrixPushEXTFunPtr

glMatrixPushEXTFunPtr :: FunPtr (GLenum -> IO ())
glMatrixPushEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixPushEXT")

{-# NOINLINE glMatrixPushEXTFunPtr #-}

-- | Usage: @'glMatrixRotatedEXT' mode angle x y z@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixRotatedEXT :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glMatrixRotatedEXT = ffienumdoubledoubledoubledoubleIOV glMatrixRotatedEXTFunPtr

glMatrixRotatedEXTFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glMatrixRotatedEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixRotatedEXT")

{-# NOINLINE glMatrixRotatedEXTFunPtr #-}

-- | Usage: @'glMatrixRotatefEXT' mode angle x y z@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixRotatefEXT :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glMatrixRotatefEXT = ffienumfloatfloatfloatfloatIOV glMatrixRotatefEXTFunPtr

glMatrixRotatefEXTFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glMatrixRotatefEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixRotatefEXT")

{-# NOINLINE glMatrixRotatefEXTFunPtr #-}

-- | Usage: @'glMatrixScaledEXT' mode x y z@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixScaledEXT :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> m ()
glMatrixScaledEXT = ffienumdoubledoubledoubleIOV glMatrixScaledEXTFunPtr

glMatrixScaledEXTFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
glMatrixScaledEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixScaledEXT")

{-# NOINLINE glMatrixScaledEXTFunPtr #-}

-- | Usage: @'glMatrixScalefEXT' mode x y z@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixScalefEXT :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> m ()
glMatrixScalefEXT = ffienumfloatfloatfloatIOV glMatrixScalefEXTFunPtr

glMatrixScalefEXTFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
glMatrixScalefEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixScalefEXT")

{-# NOINLINE glMatrixScalefEXTFunPtr #-}

-- | Usage: @'glMatrixTranslatedEXT' mode x y z@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixTranslatedEXT :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> m ()
glMatrixTranslatedEXT = ffienumdoubledoubledoubleIOV glMatrixTranslatedEXTFunPtr

glMatrixTranslatedEXTFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
glMatrixTranslatedEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixTranslatedEXT")

{-# NOINLINE glMatrixTranslatedEXTFunPtr #-}

-- | Usage: @'glMatrixTranslatefEXT' mode x y z@
--
-- The parameter @mode@ is a @MatrixMode@, one of: 'Graphics.GL.Internal.Shared.GL_MODELVIEW', 'Graphics.GL.Ext.EXT.VertexWeighting.GL_MODELVIEW0_EXT', 'Graphics.GL.Internal.Shared.GL_PROJECTION', 'Graphics.GL.Internal.Shared.GL_TEXTURE'.


glMatrixTranslatefEXT :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> m ()
glMatrixTranslatefEXT = ffienumfloatfloatfloatIOV glMatrixTranslatefEXTFunPtr

glMatrixTranslatefEXTFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
glMatrixTranslatefEXTFunPtr = unsafePerformIO (getProcAddress "glMatrixTranslatefEXT")

{-# NOINLINE glMatrixTranslatefEXTFunPtr #-}

-- | Usage: @'glMultiTexBufferEXT' texunit target internalformat buffer@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @internalformat@ is a @TypeEnum@.


glMultiTexBufferEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> m ()
glMultiTexBufferEXT = ffienumenumenumuintIOV glMultiTexBufferEXTFunPtr

glMultiTexBufferEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
glMultiTexBufferEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexBufferEXT")

{-# NOINLINE glMultiTexBufferEXTFunPtr #-}

-- | Usage: @'glMultiTexCoordPointerEXT' texunit size type stride pointer@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @type@ is a @TexCoordPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.


glMultiTexCoordPointerEXT :: MonadIO m => GLenum -> GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glMultiTexCoordPointerEXT = ffienumintenumsizeiPtrVIOV glMultiTexCoordPointerEXTFunPtr

glMultiTexCoordPointerEXTFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glMultiTexCoordPointerEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoordPointerEXT")

{-# NOINLINE glMultiTexCoordPointerEXTFunPtr #-}

-- | Usage: @'glMultiTexEnvfEXT' texunit target pname param@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureEnvTarget@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV'.
--
-- The parameter @pname@ is a @TextureEnvParameter@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_COLOR', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_MODE'.
--
-- The parameter @param@ is a @CheckedFloat32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glMultiTexEnvfvEXT'.


glMultiTexEnvfEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLfloat -> m ()
glMultiTexEnvfEXT = ffienumenumenumfloatIOV glMultiTexEnvfEXTFunPtr

glMultiTexEnvfEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
glMultiTexEnvfEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexEnvfEXT")

{-# NOINLINE glMultiTexEnvfEXTFunPtr #-}

-- | Usage: @'glMultiTexEnvfvEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureEnvTarget@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV'.
--
-- The parameter @pname@ is a @TextureEnvParameter@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_COLOR', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_MODE'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexEnvfvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glMultiTexEnvfvEXT = ffienumenumenumPtrfloatIOV glMultiTexEnvfvEXTFunPtr

glMultiTexEnvfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glMultiTexEnvfvEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexEnvfvEXT")

{-# NOINLINE glMultiTexEnvfvEXTFunPtr #-}

-- | Usage: @'glMultiTexEnviEXT' texunit target pname param@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureEnvTarget@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV'.
--
-- The parameter @pname@ is a @TextureEnvParameter@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_COLOR', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_MODE'.
--
-- The parameter @param@ is a @CheckedInt32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glMultiTexEnvivEXT'.


glMultiTexEnviEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLint -> m ()
glMultiTexEnviEXT = ffienumenumenumintIOV glMultiTexEnviEXTFunPtr

glMultiTexEnviEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
glMultiTexEnviEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexEnviEXT")

{-# NOINLINE glMultiTexEnviEXTFunPtr #-}

-- | Usage: @'glMultiTexEnvivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureEnvTarget@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV'.
--
-- The parameter @pname@ is a @TextureEnvParameter@, one of: 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_COLOR', 'Graphics.GL.Internal.Shared.GL_TEXTURE_ENV_MODE'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexEnvivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glMultiTexEnvivEXT = ffienumenumenumPtrintIOV glMultiTexEnvivEXTFunPtr

glMultiTexEnvivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glMultiTexEnvivEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexEnvivEXT")

{-# NOINLINE glMultiTexEnvivEXTFunPtr #-}

-- | Usage: @'glMultiTexGendEXT' texunit coord pname param@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glMultiTexGendvEXT'.


glMultiTexGendEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLdouble -> m ()
glMultiTexGendEXT = ffienumenumenumdoubleIOV glMultiTexGendEXTFunPtr

glMultiTexGendEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLdouble -> IO ())
glMultiTexGendEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexGendEXT")

{-# NOINLINE glMultiTexGendEXTFunPtr #-}

-- | Usage: @'glMultiTexGendvEXT' texunit coord pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexGendvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLdouble -> m ()
glMultiTexGendvEXT = ffienumenumenumPtrdoubleIOV glMultiTexGendvEXTFunPtr

glMultiTexGendvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ())
glMultiTexGendvEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexGendvEXT")

{-# NOINLINE glMultiTexGendvEXTFunPtr #-}

-- | Usage: @'glMultiTexGenfEXT' texunit coord pname param@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The parameter @param@ is a @CheckedFloat32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glMultiTexGenfvEXT'.


glMultiTexGenfEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLfloat -> m ()
glMultiTexGenfEXT = ffienumenumenumfloatIOV glMultiTexGenfEXTFunPtr

glMultiTexGenfEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
glMultiTexGenfEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexGenfEXT")

{-# NOINLINE glMultiTexGenfEXTFunPtr #-}

-- | Usage: @'glMultiTexGenfvEXT' texunit coord pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexGenfvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glMultiTexGenfvEXT = ffienumenumenumPtrfloatIOV glMultiTexGenfvEXTFunPtr

glMultiTexGenfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glMultiTexGenfvEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexGenfvEXT")

{-# NOINLINE glMultiTexGenfvEXTFunPtr #-}

-- | Usage: @'glMultiTexGeniEXT' texunit coord pname param@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The parameter @param@ is a @CheckedInt32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glMultiTexGenivEXT'.


glMultiTexGeniEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLint -> m ()
glMultiTexGeniEXT = ffienumenumenumintIOV glMultiTexGeniEXTFunPtr

glMultiTexGeniEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
glMultiTexGeniEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexGeniEXT")

{-# NOINLINE glMultiTexGeniEXTFunPtr #-}

-- | Usage: @'glMultiTexGenivEXT' texunit coord pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @pname@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexGenivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glMultiTexGenivEXT = ffienumenumenumPtrintIOV glMultiTexGenivEXTFunPtr

glMultiTexGenivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glMultiTexGenivEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexGenivEXT")

{-# NOINLINE glMultiTexGenivEXTFunPtr #-}

-- | Usage: @'glMultiTexImage1DEXT' texunit target level internalformat width border format type pixels@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureComponentCount@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width)@.


glMultiTexImage1DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glMultiTexImage1DEXT = ffienumenumintintsizeiintenumenumPtrVIOV glMultiTexImage1DEXTFunPtr

glMultiTexImage1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glMultiTexImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexImage1DEXT")

{-# NOINLINE glMultiTexImage1DEXTFunPtr #-}

-- | Usage: @'glMultiTexImage2DEXT' texunit target level internalformat width height border format type pixels@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureComponentCount@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height)@.


glMultiTexImage2DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glMultiTexImage2DEXT = ffienumenumintintsizeisizeiintenumenumPtrVIOV glMultiTexImage2DEXTFunPtr

glMultiTexImage2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glMultiTexImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexImage2DEXT")

{-# NOINLINE glMultiTexImage2DEXTFunPtr #-}

-- | Usage: @'glMultiTexImage3DEXT' texunit target level internalformat width height depth border format type pixels@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureComponentCount@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height,depth)@.


glMultiTexImage3DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glMultiTexImage3DEXT = ffienumenumintintsizeisizeisizeiintenumenumPtrVIOV glMultiTexImage3DEXTFunPtr

glMultiTexImage3DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glMultiTexImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexImage3DEXT")

{-# NOINLINE glMultiTexImage3DEXTFunPtr #-}

-- | Usage: @'glMultiTexParameterIivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexParameterIivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glMultiTexParameterIivEXT = ffienumenumenumPtrintIOV glMultiTexParameterIivEXTFunPtr

glMultiTexParameterIivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glMultiTexParameterIivEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexParameterIivEXT")

{-# NOINLINE glMultiTexParameterIivEXTFunPtr #-}

-- | Usage: @'glMultiTexParameterIuivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexParameterIuivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLuint -> m ()
glMultiTexParameterIuivEXT = ffienumenumenumPtruintIOV glMultiTexParameterIuivEXTFunPtr

glMultiTexParameterIuivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ())
glMultiTexParameterIuivEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexParameterIuivEXT")

{-# NOINLINE glMultiTexParameterIuivEXTFunPtr #-}

-- | Usage: @'glMultiTexParameterfEXT' texunit target pname param@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @param@ is a @CheckedFloat32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glMultiTexParameterfvEXT'.


glMultiTexParameterfEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLfloat -> m ()
glMultiTexParameterfEXT = ffienumenumenumfloatIOV glMultiTexParameterfEXTFunPtr

glMultiTexParameterfEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
glMultiTexParameterfEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexParameterfEXT")

{-# NOINLINE glMultiTexParameterfEXTFunPtr #-}

-- | Usage: @'glMultiTexParameterfvEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexParameterfvEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glMultiTexParameterfvEXT = ffienumenumenumPtrfloatIOV glMultiTexParameterfvEXTFunPtr

glMultiTexParameterfvEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glMultiTexParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexParameterfvEXT")

{-# NOINLINE glMultiTexParameterfvEXTFunPtr #-}

-- | Usage: @'glMultiTexParameteriEXT' texunit target pname param@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @param@ is a @CheckedInt32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glMultiTexParameterivEXT'.


glMultiTexParameteriEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLint -> m ()
glMultiTexParameteriEXT = ffienumenumenumintIOV glMultiTexParameteriEXTFunPtr

glMultiTexParameteriEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
glMultiTexParameteriEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexParameteriEXT")

{-# NOINLINE glMultiTexParameteriEXTFunPtr #-}

-- | Usage: @'glMultiTexParameterivEXT' texunit target pname params@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glMultiTexParameterivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glMultiTexParameterivEXT = ffienumenumenumPtrintIOV glMultiTexParameterivEXTFunPtr

glMultiTexParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glMultiTexParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexParameterivEXT")

{-# NOINLINE glMultiTexParameterivEXTFunPtr #-}

-- | Usage: @'glMultiTexRenderbufferEXT' texunit target renderbuffer@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.


glMultiTexRenderbufferEXT :: MonadIO m => GLenum -> GLenum -> GLuint -> m ()
glMultiTexRenderbufferEXT = ffienumenumuintIOV glMultiTexRenderbufferEXTFunPtr

glMultiTexRenderbufferEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
glMultiTexRenderbufferEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexRenderbufferEXT")

{-# NOINLINE glMultiTexRenderbufferEXTFunPtr #-}

-- | Usage: @'glMultiTexSubImage1DEXT' texunit target level xoffset width format type pixels@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width)@.


glMultiTexSubImage1DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glMultiTexSubImage1DEXT = ffienumenumintintsizeienumenumPtrVIOV glMultiTexSubImage1DEXTFunPtr

glMultiTexSubImage1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glMultiTexSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexSubImage1DEXT")

{-# NOINLINE glMultiTexSubImage1DEXTFunPtr #-}

-- | Usage: @'glMultiTexSubImage2DEXT' texunit target level xoffset yoffset width height format type pixels@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height)@.


glMultiTexSubImage2DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glMultiTexSubImage2DEXT = ffienumenumintintintsizeisizeienumenumPtrVIOV glMultiTexSubImage2DEXTFunPtr

glMultiTexSubImage2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glMultiTexSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexSubImage2DEXT")

{-# NOINLINE glMultiTexSubImage2DEXTFunPtr #-}

-- | Usage: @'glMultiTexSubImage3DEXT' texunit target level xoffset yoffset zoffset width height depth format type pixels@
--
-- The parameter @texunit@ is a @TextureUnit@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height,depth)@.


glMultiTexSubImage3DEXT :: MonadIO m => GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glMultiTexSubImage3DEXT = ffienumenumintintintintsizeisizeisizeienumenumPtrVIOV glMultiTexSubImage3DEXTFunPtr

glMultiTexSubImage3DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glMultiTexSubImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glMultiTexSubImage3DEXT")

{-# NOINLINE glMultiTexSubImage3DEXTFunPtr #-}

-- | Usage: @'glNamedBufferDataEXT' buffer size data usage@
--
-- The parameter @usage@ is a @VertexBufferObjectUsage@.
--
-- The length of @data@ should be @COMPSIZE(size)@.


glNamedBufferDataEXT :: MonadIO m => GLuint -> GLsizeiptr -> Ptr () -> GLenum -> m ()
glNamedBufferDataEXT = ffiuintsizeiptrPtrVenumIOV glNamedBufferDataEXTFunPtr

glNamedBufferDataEXTFunPtr :: FunPtr (GLuint -> GLsizeiptr -> Ptr () -> GLenum -> IO ())
glNamedBufferDataEXTFunPtr = unsafePerformIO (getProcAddress "glNamedBufferDataEXT")

{-# NOINLINE glNamedBufferDataEXTFunPtr #-}

-- | Usage: @'glNamedBufferStorageEXT' buffer size data flags@
--
-- The parameter @size@ is a @BufferSize@.
--
-- The length of @data@ should be @size@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glNamedBufferStorage'.


glNamedBufferStorageEXT :: MonadIO m => GLuint -> GLsizeiptr -> Ptr () -> GLbitfield -> m ()
glNamedBufferStorageEXT = ffiuintsizeiptrPtrVbitfieldIOV glNamedBufferStorageEXTFunPtr

glNamedBufferStorageEXTFunPtr :: FunPtr (GLuint -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ())
glNamedBufferStorageEXTFunPtr = unsafePerformIO (getProcAddress "glNamedBufferStorageEXT")

{-# NOINLINE glNamedBufferStorageEXTFunPtr #-}

-- | Usage: @'glNamedBufferSubDataEXT' buffer offset size data@
--
-- The parameter @size@ is a @BufferSize@.
--
-- The length of @data@ should be @COMPSIZE(size)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glNamedBufferSubData'.


glNamedBufferSubDataEXT :: MonadIO m => GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
glNamedBufferSubDataEXT = ffiuintintptrsizeiptrPtrVIOV glNamedBufferSubDataEXTFunPtr

glNamedBufferSubDataEXTFunPtr :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> IO ())
glNamedBufferSubDataEXTFunPtr = unsafePerformIO (getProcAddress "glNamedBufferSubDataEXT")

{-# NOINLINE glNamedBufferSubDataEXTFunPtr #-}

-- | Usage: @'glNamedCopyBufferSubDataEXT' readBuffer writeBuffer readOffset writeOffset size@


glNamedCopyBufferSubDataEXT :: MonadIO m => GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> m ()
glNamedCopyBufferSubDataEXT = ffiuintuintintptrintptrsizeiptrIOV glNamedCopyBufferSubDataEXTFunPtr

glNamedCopyBufferSubDataEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
glNamedCopyBufferSubDataEXTFunPtr = unsafePerformIO (getProcAddress "glNamedCopyBufferSubDataEXT")

{-# NOINLINE glNamedCopyBufferSubDataEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferParameteriEXT' framebuffer pname param@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @pname@ is a @FramebufferParameterName@.


glNamedFramebufferParameteriEXT :: MonadIO m => GLuint -> GLenum -> GLint -> m ()
glNamedFramebufferParameteriEXT = ffiuintenumintIOV glNamedFramebufferParameteriEXTFunPtr

glNamedFramebufferParameteriEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
glNamedFramebufferParameteriEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferParameteriEXT")

{-# NOINLINE glNamedFramebufferParameteriEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferRenderbufferEXT' framebuffer attachment renderbuffertarget renderbuffer@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @renderbuffertarget@ is a @RenderbufferTarget@.
--
-- The parameter @renderbuffer@ is a @Renderbuffer@.


glNamedFramebufferRenderbufferEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLuint -> m ()
glNamedFramebufferRenderbufferEXT = ffiuintenumenumuintIOV glNamedFramebufferRenderbufferEXTFunPtr

glNamedFramebufferRenderbufferEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
glNamedFramebufferRenderbufferEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferRenderbufferEXT")

{-# NOINLINE glNamedFramebufferRenderbufferEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferTexture1DEXT' framebuffer attachment textarget texture level@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @textarget@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.


glNamedFramebufferTexture1DEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLuint -> GLint -> m ()
glNamedFramebufferTexture1DEXT = ffiuintenumenumuintintIOV glNamedFramebufferTexture1DEXTFunPtr

glNamedFramebufferTexture1DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
glNamedFramebufferTexture1DEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferTexture1DEXT")

{-# NOINLINE glNamedFramebufferTexture1DEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferTexture2DEXT' framebuffer attachment textarget texture level@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @textarget@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.


glNamedFramebufferTexture2DEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLuint -> GLint -> m ()
glNamedFramebufferTexture2DEXT = ffiuintenumenumuintintIOV glNamedFramebufferTexture2DEXTFunPtr

glNamedFramebufferTexture2DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
glNamedFramebufferTexture2DEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferTexture2DEXT")

{-# NOINLINE glNamedFramebufferTexture2DEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferTexture3DEXT' framebuffer attachment textarget texture level zoffset@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @textarget@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.


glNamedFramebufferTexture3DEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> m ()
glNamedFramebufferTexture3DEXT = ffiuintenumenumuintintintIOV glNamedFramebufferTexture3DEXTFunPtr

glNamedFramebufferTexture3DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
glNamedFramebufferTexture3DEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferTexture3DEXT")

{-# NOINLINE glNamedFramebufferTexture3DEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferTextureEXT' framebuffer attachment texture level@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.


glNamedFramebufferTextureEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLint -> m ()
glNamedFramebufferTextureEXT = ffiuintenumuintintIOV glNamedFramebufferTextureEXTFunPtr

glNamedFramebufferTextureEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> IO ())
glNamedFramebufferTextureEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferTextureEXT")

{-# NOINLINE glNamedFramebufferTextureEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferTextureFaceEXT' framebuffer attachment texture level face@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @face@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.


glNamedFramebufferTextureFaceEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLint -> GLenum -> m ()
glNamedFramebufferTextureFaceEXT = ffiuintenumuintintenumIOV glNamedFramebufferTextureFaceEXTFunPtr

glNamedFramebufferTextureFaceEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
glNamedFramebufferTextureFaceEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferTextureFaceEXT")

{-# NOINLINE glNamedFramebufferTextureFaceEXTFunPtr #-}

-- | Usage: @'glNamedFramebufferTextureLayerEXT' framebuffer attachment texture level layer@
--
-- The parameter @framebuffer@ is a @Framebuffer@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @layer@ is a @CheckedInt32@.


glNamedFramebufferTextureLayerEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLint -> GLint -> m ()
glNamedFramebufferTextureLayerEXT = ffiuintenumuintintintIOV glNamedFramebufferTextureLayerEXTFunPtr

glNamedFramebufferTextureLayerEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ())
glNamedFramebufferTextureLayerEXTFunPtr = unsafePerformIO (getProcAddress "glNamedFramebufferTextureLayerEXT")

{-# NOINLINE glNamedFramebufferTextureLayerEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameter4dEXT' program target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glNamedProgramLocalParameter4dvEXT'.


glNamedProgramLocalParameter4dEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glNamedProgramLocalParameter4dEXT = ffiuintenumuintdoubledoubledoubledoubleIOV glNamedProgramLocalParameter4dEXTFunPtr

glNamedProgramLocalParameter4dEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glNamedProgramLocalParameter4dEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameter4dEXT")

{-# NOINLINE glNamedProgramLocalParameter4dEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameter4dvEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glNamedProgramLocalParameter4dvEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLdouble -> m ()
glNamedProgramLocalParameter4dvEXT = ffiuintenumuintPtrdoubleIOV glNamedProgramLocalParameter4dvEXTFunPtr

glNamedProgramLocalParameter4dvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ())
glNamedProgramLocalParameter4dvEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameter4dvEXT")

{-# NOINLINE glNamedProgramLocalParameter4dvEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameter4fEXT' program target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glNamedProgramLocalParameter4fvEXT'.


glNamedProgramLocalParameter4fEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glNamedProgramLocalParameter4fEXT = ffiuintenumuintfloatfloatfloatfloatIOV glNamedProgramLocalParameter4fEXTFunPtr

glNamedProgramLocalParameter4fEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glNamedProgramLocalParameter4fEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameter4fEXT")

{-# NOINLINE glNamedProgramLocalParameter4fEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameter4fvEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glNamedProgramLocalParameter4fvEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLfloat -> m ()
glNamedProgramLocalParameter4fvEXT = ffiuintenumuintPtrfloatIOV glNamedProgramLocalParameter4fvEXTFunPtr

glNamedProgramLocalParameter4fvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ())
glNamedProgramLocalParameter4fvEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameter4fvEXT")

{-# NOINLINE glNamedProgramLocalParameter4fvEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameterI4iEXT' program target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glNamedProgramLocalParameterI4ivEXT'.


glNamedProgramLocalParameterI4iEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
glNamedProgramLocalParameterI4iEXT = ffiuintenumuintintintintintIOV glNamedProgramLocalParameterI4iEXTFunPtr

glNamedProgramLocalParameterI4iEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
glNamedProgramLocalParameterI4iEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameterI4iEXT")

{-# NOINLINE glNamedProgramLocalParameterI4iEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameterI4ivEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glNamedProgramLocalParameterI4ivEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLint -> m ()
glNamedProgramLocalParameterI4ivEXT = ffiuintenumuintPtrintIOV glNamedProgramLocalParameterI4ivEXTFunPtr

glNamedProgramLocalParameterI4ivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
glNamedProgramLocalParameterI4ivEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameterI4ivEXT")

{-# NOINLINE glNamedProgramLocalParameterI4ivEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameterI4uiEXT' program target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glNamedProgramLocalParameterI4uivEXT'.


glNamedProgramLocalParameterI4uiEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glNamedProgramLocalParameterI4uiEXT = ffiuintenumuintuintuintuintuintIOV glNamedProgramLocalParameterI4uiEXTFunPtr

glNamedProgramLocalParameterI4uiEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glNamedProgramLocalParameterI4uiEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameterI4uiEXT")

{-# NOINLINE glNamedProgramLocalParameterI4uiEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameterI4uivEXT' program target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glNamedProgramLocalParameterI4uivEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr GLuint -> m ()
glNamedProgramLocalParameterI4uivEXT = ffiuintenumuintPtruintIOV glNamedProgramLocalParameterI4uivEXTFunPtr

glNamedProgramLocalParameterI4uivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ())
glNamedProgramLocalParameterI4uivEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameterI4uivEXT")

{-# NOINLINE glNamedProgramLocalParameterI4uivEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParameters4fvEXT' program target index count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count*4@.


glNamedProgramLocalParameters4fvEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glNamedProgramLocalParameters4fvEXT = ffiuintenumuintsizeiPtrfloatIOV glNamedProgramLocalParameters4fvEXTFunPtr

glNamedProgramLocalParameters4fvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glNamedProgramLocalParameters4fvEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParameters4fvEXT")

{-# NOINLINE glNamedProgramLocalParameters4fvEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParametersI4ivEXT' program target index count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count*4@.


glNamedProgramLocalParametersI4ivEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> m ()
glNamedProgramLocalParametersI4ivEXT = ffiuintenumuintsizeiPtrintIOV glNamedProgramLocalParametersI4ivEXTFunPtr

glNamedProgramLocalParametersI4ivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ())
glNamedProgramLocalParametersI4ivEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParametersI4ivEXT")

{-# NOINLINE glNamedProgramLocalParametersI4ivEXTFunPtr #-}

-- | Usage: @'glNamedProgramLocalParametersI4uivEXT' program target index count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count*4@.


glNamedProgramLocalParametersI4uivEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> m ()
glNamedProgramLocalParametersI4uivEXT = ffiuintenumuintsizeiPtruintIOV glNamedProgramLocalParametersI4uivEXTFunPtr

glNamedProgramLocalParametersI4uivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
glNamedProgramLocalParametersI4uivEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramLocalParametersI4uivEXT")

{-# NOINLINE glNamedProgramLocalParametersI4uivEXTFunPtr #-}

-- | Usage: @'glNamedProgramStringEXT' program target format len string@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The parameter @format@ is a @ProgramFormat@.
--
-- The length of @string@ should be @len@.


glNamedProgramStringEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glNamedProgramStringEXT = ffiuintenumenumsizeiPtrVIOV glNamedProgramStringEXTFunPtr

glNamedProgramStringEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glNamedProgramStringEXTFunPtr = unsafePerformIO (getProcAddress "glNamedProgramStringEXT")

{-# NOINLINE glNamedProgramStringEXTFunPtr #-}

-- | Usage: @'glNamedRenderbufferStorageEXT' renderbuffer internalformat width height@
--
-- The parameter @renderbuffer@ is a @Renderbuffer@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.


glNamedRenderbufferStorageEXT :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLsizei -> m ()
glNamedRenderbufferStorageEXT = ffiuintenumsizeisizeiIOV glNamedRenderbufferStorageEXTFunPtr

glNamedRenderbufferStorageEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> IO ())
glNamedRenderbufferStorageEXTFunPtr = unsafePerformIO (getProcAddress "glNamedRenderbufferStorageEXT")

{-# NOINLINE glNamedRenderbufferStorageEXTFunPtr #-}

-- | Usage: @'glNamedRenderbufferStorageMultisampleCoverageEXT' renderbuffer coverageSamples colorSamples internalformat width height@
--
-- The parameter @renderbuffer@ is a @Renderbuffer@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.


glNamedRenderbufferStorageMultisampleCoverageEXT :: MonadIO m => GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
glNamedRenderbufferStorageMultisampleCoverageEXT = ffiuintsizeisizeienumsizeisizeiIOV glNamedRenderbufferStorageMultisampleCoverageEXTFunPtr

glNamedRenderbufferStorageMultisampleCoverageEXTFunPtr :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
glNamedRenderbufferStorageMultisampleCoverageEXTFunPtr = unsafePerformIO (getProcAddress "glNamedRenderbufferStorageMultisampleCoverageEXT")

{-# NOINLINE glNamedRenderbufferStorageMultisampleCoverageEXTFunPtr #-}

-- | Usage: @'glNamedRenderbufferStorageMultisampleEXT' renderbuffer samples internalformat width height@
--
-- The parameter @renderbuffer@ is a @Renderbuffer@.
--
-- The parameter @internalformat@ is a @InternalFormat@, one of: 'Graphics.GL.Internal.Shared.GL_ALPHA12', 'Graphics.GL.Internal.Shared.GL_ALPHA16', @GL_ALPHA16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_ALPHA4', 'Graphics.GL.Internal.Shared.GL_ALPHA8', @GL_ALPHA_ICC_SGIX@, 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT16_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT24_SGIX', 'Graphics.GL.Ext.SGIX.DepthTexture.GL_DEPTH_COMPONENT32_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE12_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE16_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_LUMINANCE_ALPHA8_SGIS', 'Graphics.GL.Internal.Shared.GL_INTENSITY', 'Graphics.GL.Internal.Shared.GL_INTENSITY12', 'Graphics.GL.Internal.Shared.GL_INTENSITY16', @GL_INTENSITY16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_INTENSITY4', 'Graphics.GL.Internal.Shared.GL_INTENSITY8', @GL_INTENSITY_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA12', 'Graphics.GL.Internal.Shared.GL_LUMINANCE12_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16', 'Graphics.GL.Internal.Shared.GL_LUMINANCE16_ALPHA16', @GL_LUMINANCE16_ALPHA8_ICC_SGIX@, @GL_LUMINANCE16_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_LUMINANCE4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE4_ALPHA4', 'Graphics.GL.Internal.Shared.GL_LUMINANCE6_ALPHA2', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8', 'Graphics.GL.Internal.Shared.GL_LUMINANCE8_ALPHA8', @GL_LUMINANCE_ALPHA_ICC_SGIX@, @GL_LUMINANCE_ICC_SGIX@, 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_ALPHA8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_INTENSITY8_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE4_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_LUMINANCE8_SGIS', 'Graphics.GL.Internal.Shared.GL_R3_G3_B2', @GL_R5_G6_B5_A8_ICC_SGIX@, @GL_R5_G6_B5_ICC_SGIX@, 'Graphics.GL.Internal.Shared.GL_RGB10', 'Graphics.GL.Internal.Shared.GL_RGB10_A2', 'Graphics.GL.Internal.Shared.GL_RGB12', 'Graphics.GL.Internal.Shared.GL_RGB16', 'Graphics.GL.Ext.EXT.Texture.GL_RGB2_EXT', 'Graphics.GL.Internal.Shared.GL_RGB4', 'Graphics.GL.Internal.Shared.GL_RGB5', 'Graphics.GL.Internal.Shared.GL_RGB5_A1', 'Graphics.GL.Internal.Shared.GL_RGB8', 'Graphics.GL.Internal.Shared.GL_RGBA12', 'Graphics.GL.Internal.Shared.GL_RGBA16', 'Graphics.GL.Internal.Shared.GL_RGBA2', 'Graphics.GL.Internal.Shared.GL_RGBA4', 'Graphics.GL.Internal.Shared.GL_RGBA8', @GL_RGBA_ICC_SGIX@, @GL_RGB_ICC_SGIX@.


glNamedRenderbufferStorageMultisampleEXT :: MonadIO m => GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
glNamedRenderbufferStorageMultisampleEXT = ffiuintsizeienumsizeisizeiIOV glNamedRenderbufferStorageMultisampleEXTFunPtr

glNamedRenderbufferStorageMultisampleEXTFunPtr :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
glNamedRenderbufferStorageMultisampleEXTFunPtr = unsafePerformIO (getProcAddress "glNamedRenderbufferStorageMultisampleEXT")

{-# NOINLINE glNamedRenderbufferStorageMultisampleEXTFunPtr #-}

-- | Usage: @'glProgramUniform1dEXT' program location x@


glProgramUniform1dEXT :: MonadIO m => GLuint -> GLint -> GLdouble -> m ()
glProgramUniform1dEXT = ffiuintintdoubleIOV glProgramUniform1dEXTFunPtr

glProgramUniform1dEXTFunPtr :: FunPtr (GLuint -> GLint -> GLdouble -> IO ())
glProgramUniform1dEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform1dEXT")

{-# NOINLINE glProgramUniform1dEXTFunPtr #-}

-- | Usage: @'glProgramUniform1dvEXT' program location count value@
--
-- The length of @value@ should be @count@.


glProgramUniform1dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLdouble -> m ()
glProgramUniform1dvEXT = ffiuintintsizeiPtrdoubleIOV glProgramUniform1dvEXTFunPtr

glProgramUniform1dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
glProgramUniform1dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform1dvEXT")

{-# NOINLINE glProgramUniform1dvEXTFunPtr #-}

-- | Usage: @'glProgramUniform2dEXT' program location x y@


glProgramUniform2dEXT :: MonadIO m => GLuint -> GLint -> GLdouble -> GLdouble -> m ()
glProgramUniform2dEXT = ffiuintintdoubledoubleIOV glProgramUniform2dEXTFunPtr

glProgramUniform2dEXTFunPtr :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> IO ())
glProgramUniform2dEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform2dEXT")

{-# NOINLINE glProgramUniform2dEXTFunPtr #-}

-- | Usage: @'glProgramUniform2dvEXT' program location count value@
--
-- The length of @value@ should be @count*2@.


glProgramUniform2dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLdouble -> m ()
glProgramUniform2dvEXT = ffiuintintsizeiPtrdoubleIOV glProgramUniform2dvEXTFunPtr

glProgramUniform2dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
glProgramUniform2dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform2dvEXT")

{-# NOINLINE glProgramUniform2dvEXTFunPtr #-}

-- | Usage: @'glProgramUniform3dEXT' program location x y z@


glProgramUniform3dEXT :: MonadIO m => GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> m ()
glProgramUniform3dEXT = ffiuintintdoubledoubledoubleIOV glProgramUniform3dEXTFunPtr

glProgramUniform3dEXTFunPtr :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ())
glProgramUniform3dEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform3dEXT")

{-# NOINLINE glProgramUniform3dEXTFunPtr #-}

-- | Usage: @'glProgramUniform3dvEXT' program location count value@
--
-- The length of @value@ should be @count*3@.


glProgramUniform3dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLdouble -> m ()
glProgramUniform3dvEXT = ffiuintintsizeiPtrdoubleIOV glProgramUniform3dvEXTFunPtr

glProgramUniform3dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
glProgramUniform3dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform3dvEXT")

{-# NOINLINE glProgramUniform3dvEXTFunPtr #-}

-- | Usage: @'glProgramUniform4dEXT' program location x y z w@


glProgramUniform4dEXT :: MonadIO m => GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glProgramUniform4dEXT = ffiuintintdoubledoubledoubledoubleIOV glProgramUniform4dEXTFunPtr

glProgramUniform4dEXTFunPtr :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glProgramUniform4dEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform4dEXT")

{-# NOINLINE glProgramUniform4dEXTFunPtr #-}

-- | Usage: @'glProgramUniform4dvEXT' program location count value@
--
-- The length of @value@ should be @count*4@.


glProgramUniform4dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLdouble -> m ()
glProgramUniform4dvEXT = ffiuintintsizeiPtrdoubleIOV glProgramUniform4dvEXTFunPtr

glProgramUniform4dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
glProgramUniform4dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniform4dvEXT")

{-# NOINLINE glProgramUniform4dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix2dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*4@.


glProgramUniformMatrix2dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix2dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix2dvEXTFunPtr

glProgramUniformMatrix2dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix2dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix2dvEXT")

{-# NOINLINE glProgramUniformMatrix2dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix2x3dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*6@.


glProgramUniformMatrix2x3dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix2x3dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix2x3dvEXTFunPtr

glProgramUniformMatrix2x3dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix2x3dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix2x3dvEXT")

{-# NOINLINE glProgramUniformMatrix2x3dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix2x4dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*8@.


glProgramUniformMatrix2x4dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix2x4dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix2x4dvEXTFunPtr

glProgramUniformMatrix2x4dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix2x4dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix2x4dvEXT")

{-# NOINLINE glProgramUniformMatrix2x4dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix3dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*9@.


glProgramUniformMatrix3dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix3dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix3dvEXTFunPtr

glProgramUniformMatrix3dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix3dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix3dvEXT")

{-# NOINLINE glProgramUniformMatrix3dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix3x2dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*6@.


glProgramUniformMatrix3x2dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix3x2dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix3x2dvEXTFunPtr

glProgramUniformMatrix3x2dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix3x2dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix3x2dvEXT")

{-# NOINLINE glProgramUniformMatrix3x2dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix3x4dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*12@.


glProgramUniformMatrix3x4dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix3x4dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix3x4dvEXTFunPtr

glProgramUniformMatrix3x4dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix3x4dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix3x4dvEXT")

{-# NOINLINE glProgramUniformMatrix3x4dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix4dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*16@.


glProgramUniformMatrix4dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix4dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix4dvEXTFunPtr

glProgramUniformMatrix4dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix4dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix4dvEXT")

{-# NOINLINE glProgramUniformMatrix4dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix4x2dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*8@.


glProgramUniformMatrix4x2dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix4x2dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix4x2dvEXTFunPtr

glProgramUniformMatrix4x2dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix4x2dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix4x2dvEXT")

{-# NOINLINE glProgramUniformMatrix4x2dvEXTFunPtr #-}

-- | Usage: @'glProgramUniformMatrix4x3dvEXT' program location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*12@.


glProgramUniformMatrix4x3dvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
glProgramUniformMatrix4x3dvEXT = ffiuintintsizeibooleanPtrdoubleIOV glProgramUniformMatrix4x3dvEXTFunPtr

glProgramUniformMatrix4x3dvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
glProgramUniformMatrix4x3dvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramUniformMatrix4x3dvEXT")

{-# NOINLINE glProgramUniformMatrix4x3dvEXTFunPtr #-}

-- | Usage: @'glPushClientAttribDefaultEXT' mask@
--
-- The parameter @mask@ is a @ClientAttribMask@, one of: 'Graphics.GL.Internal.Shared.GL_CLIENT_ALL_ATTRIB_BITS', 'Graphics.GL.Internal.Shared.GL_CLIENT_PIXEL_STORE_BIT', 'Graphics.GL.Internal.Shared.GL_CLIENT_VERTEX_ARRAY_BIT'.


glPushClientAttribDefaultEXT :: MonadIO m => GLbitfield -> m ()
glPushClientAttribDefaultEXT = ffibitfieldIOV glPushClientAttribDefaultEXTFunPtr

glPushClientAttribDefaultEXTFunPtr :: FunPtr (GLbitfield -> IO ())
glPushClientAttribDefaultEXTFunPtr = unsafePerformIO (getProcAddress "glPushClientAttribDefaultEXT")

{-# NOINLINE glPushClientAttribDefaultEXTFunPtr #-}

-- | Usage: @'glTextureBufferEXT' texture target internalformat buffer@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.


glTextureBufferEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLuint -> m ()
glTextureBufferEXT = ffiuintenumenumuintIOV glTextureBufferEXTFunPtr

glTextureBufferEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
glTextureBufferEXTFunPtr = unsafePerformIO (getProcAddress "glTextureBufferEXT")

{-# NOINLINE glTextureBufferEXTFunPtr #-}

-- | Usage: @'glTextureBufferRangeEXT' texture target internalformat buffer offset size@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @offset@ is a @BufferOffset@.
--
-- The parameter @size@ is a @BufferSize@.


glTextureBufferRangeEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> m ()
glTextureBufferRangeEXT = ffiuintenumenumuintintptrsizeiptrIOV glTextureBufferRangeEXTFunPtr

glTextureBufferRangeEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
glTextureBufferRangeEXTFunPtr = unsafePerformIO (getProcAddress "glTextureBufferRangeEXT")

{-# NOINLINE glTextureBufferRangeEXTFunPtr #-}

-- | Usage: @'glTextureImage1DEXT' texture target level internalformat width border format type pixels@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureComponentCount@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width)@.


glTextureImage1DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glTextureImage1DEXT = ffiuintenumintintsizeiintenumenumPtrVIOV glTextureImage1DEXTFunPtr

glTextureImage1DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glTextureImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glTextureImage1DEXT")

{-# NOINLINE glTextureImage1DEXTFunPtr #-}

-- | Usage: @'glTextureImage2DEXT' texture target level internalformat width height border format type pixels@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureComponentCount@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height)@.


glTextureImage2DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glTextureImage2DEXT = ffiuintenumintintsizeisizeiintenumenumPtrVIOV glTextureImage2DEXTFunPtr

glTextureImage2DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glTextureImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glTextureImage2DEXT")

{-# NOINLINE glTextureImage2DEXTFunPtr #-}

-- | Usage: @'glTextureImage3DEXT' texture target level internalformat width height depth border format type pixels@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @internalformat@ is a @TextureComponentCount@.
--
-- The parameter @border@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height,depth)@.


glTextureImage3DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glTextureImage3DEXT = ffiuintenumintintsizeisizeisizeiintenumenumPtrVIOV glTextureImage3DEXTFunPtr

glTextureImage3DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glTextureImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glTextureImage3DEXT")

{-# NOINLINE glTextureImage3DEXTFunPtr #-}

-- | Usage: @'glTexturePageCommitmentEXT' texture level xoffset yoffset zoffset width height depth commit@


glTexturePageCommitmentEXT :: MonadIO m => GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTexturePageCommitmentEXT = ffiuintintintintintsizeisizeisizeibooleanIOV glTexturePageCommitmentEXTFunPtr

glTexturePageCommitmentEXTFunPtr :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTexturePageCommitmentEXTFunPtr = unsafePerformIO (getProcAddress "glTexturePageCommitmentEXT")

{-# NOINLINE glTexturePageCommitmentEXTFunPtr #-}

-- | Usage: @'glTextureParameterIivEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTextureParameterIivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLint -> m ()
glTextureParameterIivEXT = ffiuintenumenumPtrintIOV glTextureParameterIivEXTFunPtr

glTextureParameterIivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
glTextureParameterIivEXTFunPtr = unsafePerformIO (getProcAddress "glTextureParameterIivEXT")

{-# NOINLINE glTextureParameterIivEXTFunPtr #-}

-- | Usage: @'glTextureParameterIuivEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTextureParameterIuivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLuint -> m ()
glTextureParameterIuivEXT = ffiuintenumenumPtruintIOV glTextureParameterIuivEXTFunPtr

glTextureParameterIuivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ())
glTextureParameterIuivEXTFunPtr = unsafePerformIO (getProcAddress "glTextureParameterIuivEXT")

{-# NOINLINE glTextureParameterIuivEXTFunPtr #-}

-- | Usage: @'glTextureParameterfEXT' texture target pname param@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @param@ is a @CheckedFloat32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glTextureParameterfvEXT'.


glTextureParameterfEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLfloat -> m ()
glTextureParameterfEXT = ffiuintenumenumfloatIOV glTextureParameterfEXTFunPtr

glTextureParameterfEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLfloat -> IO ())
glTextureParameterfEXTFunPtr = unsafePerformIO (getProcAddress "glTextureParameterfEXT")

{-# NOINLINE glTextureParameterfEXTFunPtr #-}

-- | Usage: @'glTextureParameterfvEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTextureParameterfvEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glTextureParameterfvEXT = ffiuintenumenumPtrfloatIOV glTextureParameterfvEXTFunPtr

glTextureParameterfvEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glTextureParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glTextureParameterfvEXT")

{-# NOINLINE glTextureParameterfvEXTFunPtr #-}

-- | Usage: @'glTextureParameteriEXT' texture target pname param@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @param@ is a @CheckedInt32@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.DirectStateAccess.glTextureParameterivEXT'.


glTextureParameteriEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLint -> m ()
glTextureParameteriEXT = ffiuintenumenumintIOV glTextureParameteriEXTFunPtr

glTextureParameteriEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLint -> IO ())
glTextureParameteriEXTFunPtr = unsafePerformIO (getProcAddress "glTextureParameteriEXT")

{-# NOINLINE glTextureParameteriEXTFunPtr #-}

-- | Usage: @'glTextureParameterivEXT' texture target pname params@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @pname@ is a @TextureParameterName@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_LEVEL_SGIS', 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_MODE_SGIS', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_DUAL_TEXTURE_SELECT_SGIS', 'Graphics.GL.Internal.Shared.GL_GENERATE_MIPMAP', 'Graphics.GL.Ext.SGIS.GenerateMipmap.GL_GENERATE_MIPMAP_SGIS', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_BIAS_SGIX', 'Graphics.GL.Ext.SGIX.TextureScaleBias.GL_POST_TEXTURE_FILTER_SCALE_SGIX', 'Graphics.GL.Ext.SGIS.TextureSelect.GL_QUAD_TEXTURE_SELECT_SGIS', 'Graphics.GL.Ext.SGIX.ShadowAmbient.GL_SHADOW_AMBIENT_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BORDER_COLOR', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_CENTER_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_FRAME_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_OFFSET_SGIX', 'Graphics.GL.Ext.SGIX.Clipmap.GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX', 'Graphics.GL.Ext.SGIX.Shadow.GL_TEXTURE_COMPARE_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureLodBias.GL_TEXTURE_LOD_BIAS_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAG_FILTER', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_R_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_S_SGIX', 'Graphics.GL.Ext.SGIX.TextureCoordinateClamp.GL_TEXTURE_MAX_CLAMP_T_SGIX', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_FILTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_PRIORITY', 'Graphics.GL.Ext.EXT.TextureObject.GL_TEXTURE_PRIORITY_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_WRAP_Q_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_R', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_WRAP_R_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_WRAP_R_OES', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_S', 'Graphics.GL.Internal.Shared.GL_TEXTURE_WRAP_T'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glTextureParameterivEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> Ptr GLint -> m ()
glTextureParameterivEXT = ffiuintenumenumPtrintIOV glTextureParameterivEXTFunPtr

glTextureParameterivEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
glTextureParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glTextureParameterivEXT")

{-# NOINLINE glTextureParameterivEXTFunPtr #-}

-- | Usage: @'glTextureRenderbufferEXT' texture target renderbuffer@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.


glTextureRenderbufferEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> m ()
glTextureRenderbufferEXT = ffiuintenumuintIOV glTextureRenderbufferEXTFunPtr

glTextureRenderbufferEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> IO ())
glTextureRenderbufferEXTFunPtr = unsafePerformIO (getProcAddress "glTextureRenderbufferEXT")

{-# NOINLINE glTextureRenderbufferEXTFunPtr #-}

-- | Usage: @'glTextureStorage2DMultisampleEXT' texture target samples internalformat width height fixedsamplelocations@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @internalformat@ is a @TextureInternalFormat@.
--
-- The parameter @fixedsamplelocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTextureStorage2DMultisampleEXT :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> m ()
glTextureStorage2DMultisampleEXT = ffiuintenumsizeienumsizeisizeibooleanIOV glTextureStorage2DMultisampleEXTFunPtr

glTextureStorage2DMultisampleEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTextureStorage2DMultisampleEXTFunPtr = unsafePerformIO (getProcAddress "glTextureStorage2DMultisampleEXT")

{-# NOINLINE glTextureStorage2DMultisampleEXTFunPtr #-}

-- | Usage: @'glTextureStorage3DMultisampleEXT' texture target samples internalformat width height depth fixedsamplelocations@
--
-- The parameter @fixedsamplelocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTextureStorage3DMultisampleEXT :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTextureStorage3DMultisampleEXT = ffiuintenumsizeienumsizeisizeisizeibooleanIOV glTextureStorage3DMultisampleEXTFunPtr

glTextureStorage3DMultisampleEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTextureStorage3DMultisampleEXTFunPtr = unsafePerformIO (getProcAddress "glTextureStorage3DMultisampleEXT")

{-# NOINLINE glTextureStorage3DMultisampleEXTFunPtr #-}

-- | Usage: @'glTextureSubImage1DEXT' texture target level xoffset width format type pixels@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width)@.


glTextureSubImage1DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glTextureSubImage1DEXT = ffiuintenumintintsizeienumenumPtrVIOV glTextureSubImage1DEXTFunPtr

glTextureSubImage1DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glTextureSubImage1DEXTFunPtr = unsafePerformIO (getProcAddress "glTextureSubImage1DEXT")

{-# NOINLINE glTextureSubImage1DEXTFunPtr #-}

-- | Usage: @'glTextureSubImage2DEXT' texture target level xoffset yoffset width height format type pixels@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height)@.


glTextureSubImage2DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glTextureSubImage2DEXT = ffiuintenumintintintsizeisizeienumenumPtrVIOV glTextureSubImage2DEXTFunPtr

glTextureSubImage2DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glTextureSubImage2DEXTFunPtr = unsafePerformIO (getProcAddress "glTextureSubImage2DEXT")

{-# NOINLINE glTextureSubImage2DEXTFunPtr #-}

-- | Usage: @'glTextureSubImage3DEXT' texture target level xoffset yoffset zoffset width height depth format type pixels@
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @target@ is a @TextureTarget@, one of: 'Graphics.GL.Ext.SGIS.DetailTexture.GL_DETAIL_TEXTURE_2D_SGIS', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_1D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_1D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_2D', 'Graphics.GL.Ext.EXT.Texture.GL_PROXY_TEXTURE_2D_EXT', 'Graphics.GL.Internal.Shared.GL_PROXY_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_PROXY_TEXTURE_3D_EXT', 'Graphics.GL.Ext.SGIS.Texture4D.GL_PROXY_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_1D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_2D', 'Graphics.GL.Internal.Shared.GL_TEXTURE_3D', 'Graphics.GL.Ext.EXT.Texture3D.GL_TEXTURE_3D_EXT', 'Graphics.GL.Ext.OES.Texture3D.GL_TEXTURE_3D_OES', 'Graphics.GL.Ext.SGIS.Texture4D.GL_TEXTURE_4D_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_BASE_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_BASE_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LEVEL', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LEVEL_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MAX_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MAX_LOD_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_MIN_LOD', 'Graphics.GL.Ext.SGIS.TextureLod.GL_TEXTURE_MIN_LOD_SGIS'.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @xoffset@ is a @CheckedInt32@.
--
-- The parameter @yoffset@ is a @CheckedInt32@.
--
-- The parameter @zoffset@ is a @CheckedInt32@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height,depth)@.


glTextureSubImage3DEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glTextureSubImage3DEXT = ffiuintenumintintintintsizeisizeisizeienumenumPtrVIOV glTextureSubImage3DEXTFunPtr

glTextureSubImage3DEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glTextureSubImage3DEXTFunPtr = unsafePerformIO (getProcAddress "glTextureSubImage3DEXT")

{-# NOINLINE glTextureSubImage3DEXTFunPtr #-}

-- | Usage: @'glUnmapNamedBufferEXT' buffer@


glUnmapNamedBufferEXT :: MonadIO m => GLuint -> m GLboolean
glUnmapNamedBufferEXT = ffiuintIOboolean glUnmapNamedBufferEXTFunPtr

glUnmapNamedBufferEXTFunPtr :: FunPtr (GLuint -> IO GLboolean)
glUnmapNamedBufferEXTFunPtr = unsafePerformIO (getProcAddress "glUnmapNamedBufferEXT")

{-# NOINLINE glUnmapNamedBufferEXTFunPtr #-}

-- | Usage: @'glVertexArrayBindVertexBufferEXT' vaobj bindingindex buffer offset stride@
--
-- The parameter @offset@ is a @BufferOffset@.


glVertexArrayBindVertexBufferEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> m ()
glVertexArrayBindVertexBufferEXT = ffiuintuintuintintptrsizeiIOV glVertexArrayBindVertexBufferEXTFunPtr

glVertexArrayBindVertexBufferEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
glVertexArrayBindVertexBufferEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayBindVertexBufferEXT")

{-# NOINLINE glVertexArrayBindVertexBufferEXTFunPtr #-}

-- | Usage: @'glVertexArrayColorOffsetEXT' vaobj buffer size type stride offset@
--
-- The parameter @type@ is a @ColorPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT'.


glVertexArrayColorOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayColorOffsetEXT = ffiuintuintintenumsizeiintptrIOV glVertexArrayColorOffsetEXTFunPtr

glVertexArrayColorOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayColorOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayColorOffsetEXT")

{-# NOINLINE glVertexArrayColorOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayEdgeFlagOffsetEXT' vaobj buffer stride offset@


glVertexArrayEdgeFlagOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLsizei -> GLintptr -> m ()
glVertexArrayEdgeFlagOffsetEXT = ffiuintuintsizeiintptrIOV glVertexArrayEdgeFlagOffsetEXTFunPtr

glVertexArrayEdgeFlagOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> GLintptr -> IO ())
glVertexArrayEdgeFlagOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayEdgeFlagOffsetEXT")

{-# NOINLINE glVertexArrayEdgeFlagOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayFogCoordOffsetEXT' vaobj buffer type stride offset@
--
-- The parameter @type@ is a @FogCoordinatePointerType@, one of: 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_DOUBLE'.


glVertexArrayFogCoordOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayFogCoordOffsetEXT = ffiuintuintenumsizeiintptrIOV glVertexArrayFogCoordOffsetEXTFunPtr

glVertexArrayFogCoordOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayFogCoordOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayFogCoordOffsetEXT")

{-# NOINLINE glVertexArrayFogCoordOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayIndexOffsetEXT' vaobj buffer type stride offset@
--
-- The parameter @type@ is a @IndexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.


glVertexArrayIndexOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayIndexOffsetEXT = ffiuintuintenumsizeiintptrIOV glVertexArrayIndexOffsetEXTFunPtr

glVertexArrayIndexOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayIndexOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayIndexOffsetEXT")

{-# NOINLINE glVertexArrayIndexOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayMultiTexCoordOffsetEXT' vaobj buffer texunit size type stride offset@
--
-- The parameter @type@ is a @TexCoordPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.


glVertexArrayMultiTexCoordOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayMultiTexCoordOffsetEXT = ffiuintuintenumintenumsizeiintptrIOV glVertexArrayMultiTexCoordOffsetEXTFunPtr

glVertexArrayMultiTexCoordOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayMultiTexCoordOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayMultiTexCoordOffsetEXT")

{-# NOINLINE glVertexArrayMultiTexCoordOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayNormalOffsetEXT' vaobj buffer type stride offset@
--
-- The parameter @type@ is a @NormalPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.


glVertexArrayNormalOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayNormalOffsetEXT = ffiuintuintenumsizeiintptrIOV glVertexArrayNormalOffsetEXTFunPtr

glVertexArrayNormalOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayNormalOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayNormalOffsetEXT")

{-# NOINLINE glVertexArrayNormalOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArraySecondaryColorOffsetEXT' vaobj buffer size type stride offset@
--
-- The parameter @type@ is a @ColorPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT'.


glVertexArraySecondaryColorOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArraySecondaryColorOffsetEXT = ffiuintuintintenumsizeiintptrIOV glVertexArraySecondaryColorOffsetEXTFunPtr

glVertexArraySecondaryColorOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArraySecondaryColorOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArraySecondaryColorOffsetEXT")

{-# NOINLINE glVertexArraySecondaryColorOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayTexCoordOffsetEXT' vaobj buffer size type stride offset@
--
-- The parameter @type@ is a @TexCoordPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.


glVertexArrayTexCoordOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayTexCoordOffsetEXT = ffiuintuintintenumsizeiintptrIOV glVertexArrayTexCoordOffsetEXTFunPtr

glVertexArrayTexCoordOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayTexCoordOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayTexCoordOffsetEXT")

{-# NOINLINE glVertexArrayTexCoordOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribBindingEXT' vaobj attribindex bindingindex@


glVertexArrayVertexAttribBindingEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> m ()
glVertexArrayVertexAttribBindingEXT = ffiuintuintuintIOV glVertexArrayVertexAttribBindingEXTFunPtr

glVertexArrayVertexAttribBindingEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
glVertexArrayVertexAttribBindingEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribBindingEXT")

{-# NOINLINE glVertexArrayVertexAttribBindingEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribDivisorEXT' vaobj index divisor@


glVertexArrayVertexAttribDivisorEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> m ()
glVertexArrayVertexAttribDivisorEXT = ffiuintuintuintIOV glVertexArrayVertexAttribDivisorEXTFunPtr

glVertexArrayVertexAttribDivisorEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
glVertexArrayVertexAttribDivisorEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribDivisorEXT")

{-# NOINLINE glVertexArrayVertexAttribDivisorEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribFormatEXT' vaobj attribindex size type normalized relativeoffset@
--
-- The parameter @normalized@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glVertexArrayVertexAttribFormatEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> m ()
glVertexArrayVertexAttribFormatEXT = ffiuintuintintenumbooleanuintIOV glVertexArrayVertexAttribFormatEXTFunPtr

glVertexArrayVertexAttribFormatEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
glVertexArrayVertexAttribFormatEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribFormatEXT")

{-# NOINLINE glVertexArrayVertexAttribFormatEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribIFormatEXT' vaobj attribindex size type relativeoffset@


glVertexArrayVertexAttribIFormatEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLenum -> GLuint -> m ()
glVertexArrayVertexAttribIFormatEXT = ffiuintuintintenumuintIOV glVertexArrayVertexAttribIFormatEXTFunPtr

glVertexArrayVertexAttribIFormatEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
glVertexArrayVertexAttribIFormatEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribIFormatEXT")

{-# NOINLINE glVertexArrayVertexAttribIFormatEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribIOffsetEXT' vaobj buffer index size type stride offset@
--
-- The parameter @type@ is a @VertexAttribEnum@.


glVertexArrayVertexAttribIOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayVertexAttribIOffsetEXT = ffiuintuintuintintenumsizeiintptrIOV glVertexArrayVertexAttribIOffsetEXTFunPtr

glVertexArrayVertexAttribIOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayVertexAttribIOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribIOffsetEXT")

{-# NOINLINE glVertexArrayVertexAttribIOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribLFormatEXT' vaobj attribindex size type relativeoffset@


glVertexArrayVertexAttribLFormatEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLenum -> GLuint -> m ()
glVertexArrayVertexAttribLFormatEXT = ffiuintuintintenumuintIOV glVertexArrayVertexAttribLFormatEXTFunPtr

glVertexArrayVertexAttribLFormatEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
glVertexArrayVertexAttribLFormatEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribLFormatEXT")

{-# NOINLINE glVertexArrayVertexAttribLFormatEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribLOffsetEXT' vaobj buffer index size type stride offset@
--
-- The parameter @offset@ is a @BufferOffset@.


glVertexArrayVertexAttribLOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayVertexAttribLOffsetEXT = ffiuintuintuintintenumsizeiintptrIOV glVertexArrayVertexAttribLOffsetEXTFunPtr

glVertexArrayVertexAttribLOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayVertexAttribLOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribLOffsetEXT")

{-# NOINLINE glVertexArrayVertexAttribLOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexAttribOffsetEXT' vaobj buffer index size type normalized stride offset@
--
-- The parameter @type@ is a @VertexAttribPointerType@.


glVertexArrayVertexAttribOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> m ()
glVertexArrayVertexAttribOffsetEXT = ffiuintuintuintintenumbooleansizeiintptrIOV glVertexArrayVertexAttribOffsetEXTFunPtr

glVertexArrayVertexAttribOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ())
glVertexArrayVertexAttribOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexAttribOffsetEXT")

{-# NOINLINE glVertexArrayVertexAttribOffsetEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexBindingDivisorEXT' vaobj bindingindex divisor@


glVertexArrayVertexBindingDivisorEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> m ()
glVertexArrayVertexBindingDivisorEXT = ffiuintuintuintIOV glVertexArrayVertexBindingDivisorEXTFunPtr

glVertexArrayVertexBindingDivisorEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
glVertexArrayVertexBindingDivisorEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexBindingDivisorEXT")

{-# NOINLINE glVertexArrayVertexBindingDivisorEXTFunPtr #-}

-- | Usage: @'glVertexArrayVertexOffsetEXT' vaobj buffer size type stride offset@
--
-- The parameter @type@ is a @VertexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.


glVertexArrayVertexOffsetEXT :: MonadIO m => GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
glVertexArrayVertexOffsetEXT = ffiuintuintintenumsizeiintptrIOV glVertexArrayVertexOffsetEXTFunPtr

glVertexArrayVertexOffsetEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
glVertexArrayVertexOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glVertexArrayVertexOffsetEXT")

{-# NOINLINE glVertexArrayVertexOffsetEXTFunPtr #-}

pattern GL_PROGRAM_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_PROGRAM_MATRIX_EXT = 0x8E2D

pattern GL_PROGRAM_MATRIX_STACK_DEPTH_EXT  :: (Eq a, Num a) => a

pattern GL_PROGRAM_MATRIX_STACK_DEPTH_EXT = 0x8E2F

pattern GL_TRANSPOSE_PROGRAM_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSPOSE_PROGRAM_MATRIX_EXT = 0x8E2E