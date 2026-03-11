-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.BindlessTexture (
  -- * Extension Support
    gl_NV_bindless_texture

  -- * GL_NV_bindless_texture
  , glGetImageHandleNV
  , glGetTextureHandleNV
  , glGetTextureSamplerHandleNV
  , glIsImageHandleResidentNV
  , glIsTextureHandleResidentNV
  , glMakeImageHandleNonResidentNV
  , glMakeImageHandleResidentNV
  , glMakeTextureHandleNonResidentNV
  , glMakeTextureHandleResidentNV
  , glProgramUniformHandleui64NV
  , glProgramUniformHandleui64vNV
  , glUniformHandleui64NV
  , glUniformHandleui64vNV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/bindless_texture.txt GL_NV_bindless_texture> extension is available.

gl_NV_bindless_texture :: Bool
gl_NV_bindless_texture = member "GL_NV_bindless_texture" extensions
{-# NOINLINE gl_NV_bindless_texture #-}

-- | Usage: @'glGetImageHandleNV' texture level layered layer format@
--
-- The parameter @layered@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glGetImageHandleNV :: MonadIO m => GLuint -> GLint -> GLboolean -> GLint -> GLenum -> m GLuint64
glGetImageHandleNV = ffiuintintbooleanintenumIOuint64 glGetImageHandleNVFunPtr

glGetImageHandleNVFunPtr :: FunPtr (GLuint -> GLint -> GLboolean -> GLint -> GLenum -> IO GLuint64)
glGetImageHandleNVFunPtr = unsafePerformIO (getProcAddress "glGetImageHandleNV")

{-# NOINLINE glGetImageHandleNVFunPtr #-}

-- | Usage: @'glGetTextureHandleNV' texture@


glGetTextureHandleNV :: MonadIO m => GLuint -> m GLuint64
glGetTextureHandleNV = ffiuintIOuint64 glGetTextureHandleNVFunPtr

glGetTextureHandleNVFunPtr :: FunPtr (GLuint -> IO GLuint64)
glGetTextureHandleNVFunPtr = unsafePerformIO (getProcAddress "glGetTextureHandleNV")

{-# NOINLINE glGetTextureHandleNVFunPtr #-}

-- | Usage: @'glGetTextureSamplerHandleNV' texture sampler@


glGetTextureSamplerHandleNV :: MonadIO m => GLuint -> GLuint -> m GLuint64
glGetTextureSamplerHandleNV = ffiuintuintIOuint64 glGetTextureSamplerHandleNVFunPtr

glGetTextureSamplerHandleNVFunPtr :: FunPtr (GLuint -> GLuint -> IO GLuint64)
glGetTextureSamplerHandleNVFunPtr = unsafePerformIO (getProcAddress "glGetTextureSamplerHandleNV")

{-# NOINLINE glGetTextureSamplerHandleNVFunPtr #-}

-- | Usage: @'glIsImageHandleResidentNV' handle@


glIsImageHandleResidentNV :: MonadIO m => GLuint64 -> m GLboolean
glIsImageHandleResidentNV = ffiuint64IOboolean glIsImageHandleResidentNVFunPtr

glIsImageHandleResidentNVFunPtr :: FunPtr (GLuint64 -> IO GLboolean)
glIsImageHandleResidentNVFunPtr = unsafePerformIO (getProcAddress "glIsImageHandleResidentNV")

{-# NOINLINE glIsImageHandleResidentNVFunPtr #-}

-- | Usage: @'glIsTextureHandleResidentNV' handle@


glIsTextureHandleResidentNV :: MonadIO m => GLuint64 -> m GLboolean
glIsTextureHandleResidentNV = ffiuint64IOboolean glIsTextureHandleResidentNVFunPtr

glIsTextureHandleResidentNVFunPtr :: FunPtr (GLuint64 -> IO GLboolean)
glIsTextureHandleResidentNVFunPtr = unsafePerformIO (getProcAddress "glIsTextureHandleResidentNV")

{-# NOINLINE glIsTextureHandleResidentNVFunPtr #-}

-- | Usage: @'glMakeImageHandleNonResidentNV' handle@


glMakeImageHandleNonResidentNV :: MonadIO m => GLuint64 -> m ()
glMakeImageHandleNonResidentNV = ffiuint64IOV glMakeImageHandleNonResidentNVFunPtr

glMakeImageHandleNonResidentNVFunPtr :: FunPtr (GLuint64 -> IO ())
glMakeImageHandleNonResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeImageHandleNonResidentNV")

{-# NOINLINE glMakeImageHandleNonResidentNVFunPtr #-}

-- | Usage: @'glMakeImageHandleResidentNV' handle access@


glMakeImageHandleResidentNV :: MonadIO m => GLuint64 -> GLenum -> m ()
glMakeImageHandleResidentNV = ffiuint64enumIOV glMakeImageHandleResidentNVFunPtr

glMakeImageHandleResidentNVFunPtr :: FunPtr (GLuint64 -> GLenum -> IO ())
glMakeImageHandleResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeImageHandleResidentNV")

{-# NOINLINE glMakeImageHandleResidentNVFunPtr #-}

-- | Usage: @'glMakeTextureHandleNonResidentNV' handle@


glMakeTextureHandleNonResidentNV :: MonadIO m => GLuint64 -> m ()
glMakeTextureHandleNonResidentNV = ffiuint64IOV glMakeTextureHandleNonResidentNVFunPtr

glMakeTextureHandleNonResidentNVFunPtr :: FunPtr (GLuint64 -> IO ())
glMakeTextureHandleNonResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeTextureHandleNonResidentNV")

{-# NOINLINE glMakeTextureHandleNonResidentNVFunPtr #-}

-- | Usage: @'glMakeTextureHandleResidentNV' handle@


glMakeTextureHandleResidentNV :: MonadIO m => GLuint64 -> m ()
glMakeTextureHandleResidentNV = ffiuint64IOV glMakeTextureHandleResidentNVFunPtr

glMakeTextureHandleResidentNVFunPtr :: FunPtr (GLuint64 -> IO ())
glMakeTextureHandleResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeTextureHandleResidentNV")

{-# NOINLINE glMakeTextureHandleResidentNVFunPtr #-}

-- | Usage: @'glProgramUniformHandleui64NV' program location value@


glProgramUniformHandleui64NV :: MonadIO m => GLuint -> GLint -> GLuint64 -> m ()
glProgramUniformHandleui64NV = ffiuintintuint64IOV glProgramUniformHandleui64NVFunPtr

glProgramUniformHandleui64NVFunPtr :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
glProgramUniformHandleui64NVFunPtr = unsafePerformIO (getProcAddress "glProgramUniformHandleui64NV")

{-# NOINLINE glProgramUniformHandleui64NVFunPtr #-}

-- | Usage: @'glProgramUniformHandleui64vNV' program location count values@
--
-- The length of @values@ should be @count@.


glProgramUniformHandleui64vNV :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glProgramUniformHandleui64vNV = ffiuintintsizeiPtruint64IOV glProgramUniformHandleui64vNVFunPtr

glProgramUniformHandleui64vNVFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glProgramUniformHandleui64vNVFunPtr = unsafePerformIO (getProcAddress "glProgramUniformHandleui64vNV")

{-# NOINLINE glProgramUniformHandleui64vNVFunPtr #-}

-- | Usage: @'glUniformHandleui64NV' location value@


glUniformHandleui64NV :: MonadIO m => GLint -> GLuint64 -> m ()
glUniformHandleui64NV = ffiintuint64IOV glUniformHandleui64NVFunPtr

glUniformHandleui64NVFunPtr :: FunPtr (GLint -> GLuint64 -> IO ())
glUniformHandleui64NVFunPtr = unsafePerformIO (getProcAddress "glUniformHandleui64NV")

{-# NOINLINE glUniformHandleui64NVFunPtr #-}

-- | Usage: @'glUniformHandleui64vNV' location count value@
--
-- The length of @value@ should be @count@.


glUniformHandleui64vNV :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64 -> m ()
glUniformHandleui64vNV = ffiintsizeiPtruint64IOV glUniformHandleui64vNVFunPtr

glUniformHandleui64vNVFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glUniformHandleui64vNVFunPtr = unsafePerformIO (getProcAddress "glUniformHandleui64vNV")

{-# NOINLINE glUniformHandleui64vNVFunPtr #-}