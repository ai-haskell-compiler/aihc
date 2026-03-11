-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.Texture3D (
  -- * Extension Support
    gl_OES_texture_3D

  -- * GL_OES_texture_3D
  , glCompressedTexImage3DOES
  , glCompressedTexSubImage3DOES
  , glCopyTexSubImage3DOES
  , glFramebufferTexture3DOES
  , glTexImage3DOES
  , glTexSubImage3DOES
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_OES
  , pattern GL_MAX_3D_TEXTURE_SIZE_OES
  , pattern GL_SAMPLER_3D_OES
  , pattern GL_TEXTURE_3D_OES
  , pattern GL_TEXTURE_BINDING_3D_OES
  , pattern GL_TEXTURE_WRAP_R_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_3D.txt GL_OES_texture_3D> extension is available.

gl_OES_texture_3D :: Bool
gl_OES_texture_3D = member "GL_OES_texture_3D" extensions
{-# NOINLINE gl_OES_texture_3D #-}

-- | Usage: @'glCompressedTexImage3DOES' target level internalformat width height depth border imageSize data@
--
-- The length of @data@ should be @imageSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCompressedTexImage3D'.


glCompressedTexImage3DOES :: MonadIO m => GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
glCompressedTexImage3DOES = ffienumintenumsizeisizeisizeiintsizeiPtrVIOV glCompressedTexImage3DOESFunPtr

glCompressedTexImage3DOESFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
glCompressedTexImage3DOESFunPtr = unsafePerformIO (getProcAddress "glCompressedTexImage3DOES")

{-# NOINLINE glCompressedTexImage3DOESFunPtr #-}

-- | Usage: @'glCompressedTexSubImage3DOES' target level xoffset yoffset zoffset width height depth format imageSize data@
--
-- The length of @data@ should be @imageSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCompressedTexSubImage3D'.


glCompressedTexSubImage3DOES :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
glCompressedTexSubImage3DOES = ffienumintintintintsizeisizeisizeienumsizeiPtrVIOV glCompressedTexSubImage3DOESFunPtr

glCompressedTexSubImage3DOESFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
glCompressedTexSubImage3DOESFunPtr = unsafePerformIO (getProcAddress "glCompressedTexSubImage3DOES")

{-# NOINLINE glCompressedTexSubImage3DOESFunPtr #-}

-- | Usage: @'glCopyTexSubImage3DOES' target level xoffset yoffset zoffset x y width height@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyTexSubImage3D'.


glCopyTexSubImage3DOES :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glCopyTexSubImage3DOES = ffienumintintintintintintsizeisizeiIOV glCopyTexSubImage3DOESFunPtr

glCopyTexSubImage3DOESFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glCopyTexSubImage3DOESFunPtr = unsafePerformIO (getProcAddress "glCopyTexSubImage3DOES")

{-# NOINLINE glCopyTexSubImage3DOESFunPtr #-}

-- | Usage: @'glFramebufferTexture3DOES' target attachment textarget texture level zoffset@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFramebufferTexture3D'.


glFramebufferTexture3DOES :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> m ()
glFramebufferTexture3DOES = ffienumenumenumuintintintIOV glFramebufferTexture3DOESFunPtr

glFramebufferTexture3DOESFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
glFramebufferTexture3DOESFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture3DOES")

{-# NOINLINE glFramebufferTexture3DOESFunPtr #-}

-- | Usage: @'glTexImage3DOES' target level internalformat width height depth border format type pixels@
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height,depth)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexImage3D'.


glTexImage3DOES :: MonadIO m => GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glTexImage3DOES = ffienumintenumsizeisizeisizeiintenumenumPtrVIOV glTexImage3DOESFunPtr

glTexImage3DOESFunPtr :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glTexImage3DOESFunPtr = unsafePerformIO (getProcAddress "glTexImage3DOES")

{-# NOINLINE glTexImage3DOESFunPtr #-}

-- | Usage: @'glTexSubImage3DOES' target level xoffset yoffset zoffset width height depth format type pixels@
--
-- The length of @pixels@ should be @COMPSIZE(format,type,width,height,depth)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexSubImage3D'.


glTexSubImage3DOES :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glTexSubImage3DOES = ffienumintintintintsizeisizeisizeienumenumPtrVIOV glTexSubImage3DOESFunPtr

glTexSubImage3DOESFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glTexSubImage3DOESFunPtr = unsafePerformIO (getProcAddress "glTexSubImage3DOES")

{-# NOINLINE glTexSubImage3DOESFunPtr #-}

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_OES  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_OES = 0x8CD4

pattern GL_MAX_3D_TEXTURE_SIZE_OES  :: (Eq a, Num a) => a

pattern GL_MAX_3D_TEXTURE_SIZE_OES = 0x8073

pattern GL_SAMPLER_3D_OES  :: (Eq a, Num a) => a

pattern GL_SAMPLER_3D_OES = 0x8B5F

pattern GL_TEXTURE_3D_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_3D_OES = 0x806F

pattern GL_TEXTURE_BINDING_3D_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_3D_OES = 0x806A

pattern GL_TEXTURE_WRAP_R_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_WRAP_R_OES = 0x8072