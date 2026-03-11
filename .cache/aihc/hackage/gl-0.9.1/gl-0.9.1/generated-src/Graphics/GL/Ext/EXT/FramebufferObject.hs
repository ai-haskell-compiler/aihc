-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FramebufferObject (
  -- * Extension Support
    gl_EXT_framebuffer_object

  -- * GL_EXT_framebuffer_object
  , glBindFramebufferEXT
  , glBindRenderbufferEXT
  , glCheckFramebufferStatusEXT
  , glDeleteFramebuffersEXT
  , glDeleteRenderbuffersEXT
  , glFramebufferRenderbufferEXT
  , glFramebufferTexture1DEXT
  , glFramebufferTexture2DEXT
  , glFramebufferTexture3DEXT
  , glGenFramebuffersEXT
  , glGenRenderbuffersEXT
  , glGenerateMipmapEXT
  , glGetFramebufferAttachmentParameterivEXT
  , glGetRenderbufferParameterivEXT
  , glIsFramebufferEXT
  , glIsRenderbufferEXT
  , glRenderbufferStorageEXT
  , pattern GL_COLOR_ATTACHMENT0_EXT
  , pattern GL_COLOR_ATTACHMENT10_EXT
  , pattern GL_COLOR_ATTACHMENT11_EXT
  , pattern GL_COLOR_ATTACHMENT12_EXT
  , pattern GL_COLOR_ATTACHMENT13_EXT
  , pattern GL_COLOR_ATTACHMENT14_EXT
  , pattern GL_COLOR_ATTACHMENT15_EXT
  , pattern GL_COLOR_ATTACHMENT1_EXT
  , pattern GL_COLOR_ATTACHMENT2_EXT
  , pattern GL_COLOR_ATTACHMENT3_EXT
  , pattern GL_COLOR_ATTACHMENT4_EXT
  , pattern GL_COLOR_ATTACHMENT5_EXT
  , pattern GL_COLOR_ATTACHMENT6_EXT
  , pattern GL_COLOR_ATTACHMENT7_EXT
  , pattern GL_COLOR_ATTACHMENT8_EXT
  , pattern GL_COLOR_ATTACHMENT9_EXT
  , pattern GL_DEPTH_ATTACHMENT_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT
  , pattern GL_FRAMEBUFFER_BINDING_EXT
  , pattern GL_FRAMEBUFFER_COMPLETE_EXT
  , pattern GL_FRAMEBUFFER_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT
  , pattern GL_FRAMEBUFFER_UNSUPPORTED_EXT
  , pattern GL_INVALID_FRAMEBUFFER_OPERATION_EXT
  , pattern GL_MAX_COLOR_ATTACHMENTS_EXT
  , pattern GL_MAX_RENDERBUFFER_SIZE_EXT
  , pattern GL_RENDERBUFFER_ALPHA_SIZE_EXT
  , pattern GL_RENDERBUFFER_BINDING_EXT
  , pattern GL_RENDERBUFFER_BLUE_SIZE_EXT
  , pattern GL_RENDERBUFFER_DEPTH_SIZE_EXT
  , pattern GL_RENDERBUFFER_EXT
  , pattern GL_RENDERBUFFER_GREEN_SIZE_EXT
  , pattern GL_RENDERBUFFER_HEIGHT_EXT
  , pattern GL_RENDERBUFFER_INTERNAL_FORMAT_EXT
  , pattern GL_RENDERBUFFER_RED_SIZE_EXT
  , pattern GL_RENDERBUFFER_STENCIL_SIZE_EXT
  , pattern GL_RENDERBUFFER_WIDTH_EXT
  , pattern GL_STENCIL_ATTACHMENT_EXT
  , pattern GL_STENCIL_INDEX16_EXT
  , pattern GL_STENCIL_INDEX1_EXT
  , pattern GL_STENCIL_INDEX4_EXT
  , pattern GL_STENCIL_INDEX8_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/framebuffer_object.txt GL_EXT_framebuffer_object> extension is available.

gl_EXT_framebuffer_object :: Bool
gl_EXT_framebuffer_object = member "GL_EXT_framebuffer_object" extensions
{-# NOINLINE gl_EXT_framebuffer_object #-}

-- | Usage: @'glBindFramebufferEXT' target framebuffer@
--
-- The parameter @target@ is a @FramebufferTarget@.


glBindFramebufferEXT :: MonadIO m => GLenum -> GLuint -> m ()
glBindFramebufferEXT = ffienumuintIOV glBindFramebufferEXTFunPtr

glBindFramebufferEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindFramebufferEXTFunPtr = unsafePerformIO (getProcAddress "glBindFramebufferEXT")

{-# NOINLINE glBindFramebufferEXTFunPtr #-}

-- | Usage: @'glBindRenderbufferEXT' target renderbuffer@
--
-- The parameter @target@ is a @RenderbufferTarget@.


glBindRenderbufferEXT :: MonadIO m => GLenum -> GLuint -> m ()
glBindRenderbufferEXT = ffienumuintIOV glBindRenderbufferEXTFunPtr

glBindRenderbufferEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindRenderbufferEXTFunPtr = unsafePerformIO (getProcAddress "glBindRenderbufferEXT")

{-# NOINLINE glBindRenderbufferEXTFunPtr #-}

-- | Usage: @'glCheckFramebufferStatusEXT' target@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCheckFramebufferStatus'.


glCheckFramebufferStatusEXT :: MonadIO m => GLenum -> m GLenum
glCheckFramebufferStatusEXT = ffienumIOenum glCheckFramebufferStatusEXTFunPtr

glCheckFramebufferStatusEXTFunPtr :: FunPtr (GLenum -> IO GLenum)
glCheckFramebufferStatusEXTFunPtr = unsafePerformIO (getProcAddress "glCheckFramebufferStatusEXT")

{-# NOINLINE glCheckFramebufferStatusEXTFunPtr #-}

-- | Usage: @'glDeleteFramebuffersEXT' n framebuffers@
--
-- The length of @framebuffers@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteFramebuffers'.


glDeleteFramebuffersEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteFramebuffersEXT = ffisizeiPtruintIOV glDeleteFramebuffersEXTFunPtr

glDeleteFramebuffersEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteFramebuffersEXTFunPtr = unsafePerformIO (getProcAddress "glDeleteFramebuffersEXT")

{-# NOINLINE glDeleteFramebuffersEXTFunPtr #-}

-- | Usage: @'glDeleteRenderbuffersEXT' n renderbuffers@
--
-- The length of @renderbuffers@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteRenderbuffers'.


glDeleteRenderbuffersEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteRenderbuffersEXT = ffisizeiPtruintIOV glDeleteRenderbuffersEXTFunPtr

glDeleteRenderbuffersEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteRenderbuffersEXTFunPtr = unsafePerformIO (getProcAddress "glDeleteRenderbuffersEXT")

{-# NOINLINE glDeleteRenderbuffersEXTFunPtr #-}

-- | Usage: @'glFramebufferRenderbufferEXT' target attachment renderbuffertarget renderbuffer@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @renderbuffertarget@ is a @RenderbufferTarget@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFramebufferRenderbuffer'.


glFramebufferRenderbufferEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> m ()
glFramebufferRenderbufferEXT = ffienumenumenumuintIOV glFramebufferRenderbufferEXTFunPtr

glFramebufferRenderbufferEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
glFramebufferRenderbufferEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferRenderbufferEXT")

{-# NOINLINE glFramebufferRenderbufferEXTFunPtr #-}

-- | Usage: @'glFramebufferTexture1DEXT' target attachment textarget texture level@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFramebufferTexture1D'.


glFramebufferTexture1DEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> m ()
glFramebufferTexture1DEXT = ffienumenumenumuintintIOV glFramebufferTexture1DEXTFunPtr

glFramebufferTexture1DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
glFramebufferTexture1DEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture1DEXT")

{-# NOINLINE glFramebufferTexture1DEXTFunPtr #-}

-- | Usage: @'glFramebufferTexture2DEXT' target attachment textarget texture level@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFramebufferTexture2D'.


glFramebufferTexture2DEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> m ()
glFramebufferTexture2DEXT = ffienumenumenumuintintIOV glFramebufferTexture2DEXTFunPtr

glFramebufferTexture2DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
glFramebufferTexture2DEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture2DEXT")

{-# NOINLINE glFramebufferTexture2DEXTFunPtr #-}

-- | Usage: @'glFramebufferTexture3DEXT' target attachment textarget texture level zoffset@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFramebufferTexture3D'.


glFramebufferTexture3DEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> m ()
glFramebufferTexture3DEXT = ffienumenumenumuintintintIOV glFramebufferTexture3DEXTFunPtr

glFramebufferTexture3DEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
glFramebufferTexture3DEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture3DEXT")

{-# NOINLINE glFramebufferTexture3DEXTFunPtr #-}

-- | Usage: @'glGenFramebuffersEXT' n framebuffers@
--
-- The length of @framebuffers@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenFramebuffers'.


glGenFramebuffersEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenFramebuffersEXT = ffisizeiPtruintIOV glGenFramebuffersEXTFunPtr

glGenFramebuffersEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenFramebuffersEXTFunPtr = unsafePerformIO (getProcAddress "glGenFramebuffersEXT")

{-# NOINLINE glGenFramebuffersEXTFunPtr #-}

-- | Usage: @'glGenRenderbuffersEXT' n renderbuffers@
--
-- The length of @renderbuffers@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenRenderbuffers'.


glGenRenderbuffersEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenRenderbuffersEXT = ffisizeiPtruintIOV glGenRenderbuffersEXTFunPtr

glGenRenderbuffersEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenRenderbuffersEXTFunPtr = unsafePerformIO (getProcAddress "glGenRenderbuffersEXT")

{-# NOINLINE glGenRenderbuffersEXTFunPtr #-}

-- | Usage: @'glGenerateMipmapEXT' target@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenerateMipmap'.


glGenerateMipmapEXT :: MonadIO m => GLenum -> m ()
glGenerateMipmapEXT = ffienumIOV glGenerateMipmapEXTFunPtr

glGenerateMipmapEXTFunPtr :: FunPtr (GLenum -> IO ())
glGenerateMipmapEXTFunPtr = unsafePerformIO (getProcAddress "glGenerateMipmapEXT")

{-# NOINLINE glGenerateMipmapEXTFunPtr #-}

-- | Usage: @'glGetFramebufferAttachmentParameterivEXT' target attachment pname params@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetFramebufferAttachmentParameteriv'.


glGetFramebufferAttachmentParameterivEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetFramebufferAttachmentParameterivEXT = ffienumenumenumPtrintIOV glGetFramebufferAttachmentParameterivEXTFunPtr

glGetFramebufferAttachmentParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetFramebufferAttachmentParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetFramebufferAttachmentParameterivEXT")

{-# NOINLINE glGetFramebufferAttachmentParameterivEXTFunPtr #-}

-- | Usage: @'glGetRenderbufferParameterivEXT' target pname params@
--
-- The parameter @target@ is a @RenderbufferTarget@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetRenderbufferParameteriv'.


glGetRenderbufferParameterivEXT :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetRenderbufferParameterivEXT = ffienumenumPtrintIOV glGetRenderbufferParameterivEXTFunPtr

glGetRenderbufferParameterivEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetRenderbufferParameterivEXTFunPtr = unsafePerformIO (getProcAddress "glGetRenderbufferParameterivEXT")

{-# NOINLINE glGetRenderbufferParameterivEXTFunPtr #-}

-- | Usage: @'glIsFramebufferEXT' framebuffer@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsFramebuffer'.


glIsFramebufferEXT :: MonadIO m => GLuint -> m GLboolean
glIsFramebufferEXT = ffiuintIOboolean glIsFramebufferEXTFunPtr

glIsFramebufferEXTFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsFramebufferEXTFunPtr = unsafePerformIO (getProcAddress "glIsFramebufferEXT")

{-# NOINLINE glIsFramebufferEXTFunPtr #-}

-- | Usage: @'glIsRenderbufferEXT' renderbuffer@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsRenderbuffer'.


glIsRenderbufferEXT :: MonadIO m => GLuint -> m GLboolean
glIsRenderbufferEXT = ffiuintIOboolean glIsRenderbufferEXTFunPtr

glIsRenderbufferEXTFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsRenderbufferEXTFunPtr = unsafePerformIO (getProcAddress "glIsRenderbufferEXT")

{-# NOINLINE glIsRenderbufferEXTFunPtr #-}

-- | Usage: @'glRenderbufferStorageEXT' target internalformat width height@
--
-- The parameter @target@ is a @RenderbufferTarget@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glRenderbufferStorage'.


glRenderbufferStorageEXT :: MonadIO m => GLenum -> GLenum -> GLsizei -> GLsizei -> m ()
glRenderbufferStorageEXT = ffienumenumsizeisizeiIOV glRenderbufferStorageEXTFunPtr

glRenderbufferStorageEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
glRenderbufferStorageEXTFunPtr = unsafePerformIO (getProcAddress "glRenderbufferStorageEXT")

{-# NOINLINE glRenderbufferStorageEXTFunPtr #-}

pattern GL_DEPTH_ATTACHMENT_EXT  :: (Eq a, Num a) => a

pattern GL_DEPTH_ATTACHMENT_EXT = 0x8D00

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT = 0x8CD1

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT = 0x8CD0

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = 0x8CD4

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = 0x8CD3

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT = 0x8CD2

pattern GL_FRAMEBUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_BINDING_EXT = 0x8CA6

pattern GL_FRAMEBUFFER_COMPLETE_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_COMPLETE_EXT = 0x8CD5

pattern GL_FRAMEBUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_EXT = 0x8D40

pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT = 0x8CD6

pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT = 0x8CD9

pattern GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT = 0x8CDB

pattern GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT = 0x8CDA

pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = 0x8CD7

pattern GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT = 0x8CDC

pattern GL_FRAMEBUFFER_UNSUPPORTED_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_UNSUPPORTED_EXT = 0x8CDD

pattern GL_INVALID_FRAMEBUFFER_OPERATION_EXT  :: (Eq a, Num a) => a

pattern GL_INVALID_FRAMEBUFFER_OPERATION_EXT = 0x0506

pattern GL_MAX_RENDERBUFFER_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_RENDERBUFFER_SIZE_EXT = 0x84E8

pattern GL_RENDERBUFFER_ALPHA_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_ALPHA_SIZE_EXT = 0x8D53

pattern GL_RENDERBUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_BINDING_EXT = 0x8CA7

pattern GL_RENDERBUFFER_BLUE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_BLUE_SIZE_EXT = 0x8D52

pattern GL_RENDERBUFFER_DEPTH_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_DEPTH_SIZE_EXT = 0x8D54

pattern GL_RENDERBUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_EXT = 0x8D41

pattern GL_RENDERBUFFER_GREEN_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_GREEN_SIZE_EXT = 0x8D51

pattern GL_RENDERBUFFER_HEIGHT_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_HEIGHT_EXT = 0x8D43

pattern GL_RENDERBUFFER_INTERNAL_FORMAT_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_INTERNAL_FORMAT_EXT = 0x8D44

pattern GL_RENDERBUFFER_RED_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_RED_SIZE_EXT = 0x8D50

pattern GL_RENDERBUFFER_STENCIL_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_STENCIL_SIZE_EXT = 0x8D55

pattern GL_RENDERBUFFER_WIDTH_EXT  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_WIDTH_EXT = 0x8D42

pattern GL_STENCIL_ATTACHMENT_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_ATTACHMENT_EXT = 0x8D20

pattern GL_STENCIL_INDEX16_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_INDEX16_EXT = 0x8D49

pattern GL_STENCIL_INDEX1_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_INDEX1_EXT = 0x8D46

pattern GL_STENCIL_INDEX4_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_INDEX4_EXT = 0x8D47

pattern GL_STENCIL_INDEX8_EXT  :: (Eq a, Num a) => a

pattern GL_STENCIL_INDEX8_EXT = 0x8D48