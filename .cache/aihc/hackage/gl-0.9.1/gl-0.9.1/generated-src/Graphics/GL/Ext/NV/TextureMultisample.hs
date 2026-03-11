-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureMultisample (
  -- * Extension Support
    gl_NV_texture_multisample

  -- * GL_NV_texture_multisample
  , glTexImage2DMultisampleCoverageNV
  , glTexImage3DMultisampleCoverageNV
  , glTextureImage2DMultisampleCoverageNV
  , glTextureImage2DMultisampleNV
  , glTextureImage3DMultisampleCoverageNV
  , glTextureImage3DMultisampleNV
  , pattern GL_TEXTURE_COLOR_SAMPLES_NV
  , pattern GL_TEXTURE_COVERAGE_SAMPLES_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_multisample.txt GL_NV_texture_multisample> extension is available.

gl_NV_texture_multisample :: Bool
gl_NV_texture_multisample = member "GL_NV_texture_multisample" extensions
{-# NOINLINE gl_NV_texture_multisample #-}

-- | Usage: @'glTexImage2DMultisampleCoverageNV' target coverageSamples colorSamples internalFormat width height fixedSampleLocations@
--
-- The parameter @fixedSampleLocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTexImage2DMultisampleCoverageNV :: MonadIO m => GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> m ()
glTexImage2DMultisampleCoverageNV = ffienumsizeisizeiintsizeisizeibooleanIOV glTexImage2DMultisampleCoverageNVFunPtr

glTexImage2DMultisampleCoverageNVFunPtr :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTexImage2DMultisampleCoverageNVFunPtr = unsafePerformIO (getProcAddress "glTexImage2DMultisampleCoverageNV")

{-# NOINLINE glTexImage2DMultisampleCoverageNVFunPtr #-}

-- | Usage: @'glTexImage3DMultisampleCoverageNV' target coverageSamples colorSamples internalFormat width height depth fixedSampleLocations@
--
-- The parameter @fixedSampleLocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTexImage3DMultisampleCoverageNV :: MonadIO m => GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTexImage3DMultisampleCoverageNV = ffienumsizeisizeiintsizeisizeisizeibooleanIOV glTexImage3DMultisampleCoverageNVFunPtr

glTexImage3DMultisampleCoverageNVFunPtr :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTexImage3DMultisampleCoverageNVFunPtr = unsafePerformIO (getProcAddress "glTexImage3DMultisampleCoverageNV")

{-# NOINLINE glTexImage3DMultisampleCoverageNVFunPtr #-}

-- | Usage: @'glTextureImage2DMultisampleCoverageNV' texture target coverageSamples colorSamples internalFormat width height fixedSampleLocations@
--
-- The parameter @fixedSampleLocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTextureImage2DMultisampleCoverageNV :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> m ()
glTextureImage2DMultisampleCoverageNV = ffiuintenumsizeisizeiintsizeisizeibooleanIOV glTextureImage2DMultisampleCoverageNVFunPtr

glTextureImage2DMultisampleCoverageNVFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTextureImage2DMultisampleCoverageNVFunPtr = unsafePerformIO (getProcAddress "glTextureImage2DMultisampleCoverageNV")

{-# NOINLINE glTextureImage2DMultisampleCoverageNVFunPtr #-}

-- | Usage: @'glTextureImage2DMultisampleNV' texture target samples internalFormat width height fixedSampleLocations@
--
-- The parameter @fixedSampleLocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTextureImage2DMultisampleNV :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> m ()
glTextureImage2DMultisampleNV = ffiuintenumsizeiintsizeisizeibooleanIOV glTextureImage2DMultisampleNVFunPtr

glTextureImage2DMultisampleNVFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTextureImage2DMultisampleNVFunPtr = unsafePerformIO (getProcAddress "glTextureImage2DMultisampleNV")

{-# NOINLINE glTextureImage2DMultisampleNVFunPtr #-}

-- | Usage: @'glTextureImage3DMultisampleCoverageNV' texture target coverageSamples colorSamples internalFormat width height depth fixedSampleLocations@
--
-- The parameter @fixedSampleLocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTextureImage3DMultisampleCoverageNV :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTextureImage3DMultisampleCoverageNV = ffiuintenumsizeisizeiintsizeisizeisizeibooleanIOV glTextureImage3DMultisampleCoverageNVFunPtr

glTextureImage3DMultisampleCoverageNVFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTextureImage3DMultisampleCoverageNVFunPtr = unsafePerformIO (getProcAddress "glTextureImage3DMultisampleCoverageNV")

{-# NOINLINE glTextureImage3DMultisampleCoverageNVFunPtr #-}

-- | Usage: @'glTextureImage3DMultisampleNV' texture target samples internalFormat width height depth fixedSampleLocations@
--
-- The parameter @fixedSampleLocations@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glTextureImage3DMultisampleNV :: MonadIO m => GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
glTextureImage3DMultisampleNV = ffiuintenumsizeiintsizeisizeisizeibooleanIOV glTextureImage3DMultisampleNVFunPtr

glTextureImage3DMultisampleNVFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
glTextureImage3DMultisampleNVFunPtr = unsafePerformIO (getProcAddress "glTextureImage3DMultisampleNV")

{-# NOINLINE glTextureImage3DMultisampleNVFunPtr #-}

pattern GL_TEXTURE_COLOR_SAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COLOR_SAMPLES_NV = 0x9046

pattern GL_TEXTURE_COVERAGE_SAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COVERAGE_SAMPLES_NV = 0x9045