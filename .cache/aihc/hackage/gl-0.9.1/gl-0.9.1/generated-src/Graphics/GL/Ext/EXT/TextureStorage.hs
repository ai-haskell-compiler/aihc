-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureStorage (
  -- * Extension Support
    gl_EXT_texture_storage

  -- * GL_EXT_texture_storage
  , glTexStorage1DEXT
  , glTexStorage2DEXT
  , glTexStorage3DEXT
  , glTextureStorage1DEXT
  , glTextureStorage2DEXT
  , glTextureStorage3DEXT
  , pattern GL_ALPHA16F_EXT
  , pattern GL_ALPHA32F_EXT
  , pattern GL_ALPHA8_EXT
  , pattern GL_BGRA8_EXT
  , pattern GL_LUMINANCE16F_EXT
  , pattern GL_LUMINANCE32F_EXT
  , pattern GL_LUMINANCE8_ALPHA8_EXT
  , pattern GL_LUMINANCE8_EXT
  , pattern GL_LUMINANCE_ALPHA16F_EXT
  , pattern GL_LUMINANCE_ALPHA32F_EXT
  , pattern GL_R16F_EXT
  , pattern GL_R32F_EXT
  , pattern GL_R8_EXT
  , pattern GL_RG16F_EXT
  , pattern GL_RG32F_EXT
  , pattern GL_RG8_EXT
  , pattern GL_RGB10_A2_EXT
  , pattern GL_RGB10_EXT
  , pattern GL_RGB16F_EXT
  , pattern GL_RGB32F_EXT
  , pattern GL_RGBA16F_EXT
  , pattern GL_RGBA32F_EXT
  , pattern GL_TEXTURE_IMMUTABLE_FORMAT_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_storage.txt GL_EXT_texture_storage> extension is available.

gl_EXT_texture_storage :: Bool
gl_EXT_texture_storage = member "GL_EXT_texture_storage" extensions
{-# NOINLINE gl_EXT_texture_storage #-}

-- | Usage: @'glTexStorage1DEXT' target levels internalformat width@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexStorage1D'.


glTexStorage1DEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> m ()
glTexStorage1DEXT = ffienumsizeienumsizeiIOV glTexStorage1DEXTFunPtr

glTexStorage1DEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> IO ())
glTexStorage1DEXTFunPtr = unsafePerformIO (getProcAddress "glTexStorage1DEXT")

{-# NOINLINE glTexStorage1DEXTFunPtr #-}

-- | Usage: @'glTexStorage2DEXT' target levels internalformat width height@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexStorage2D'.


glTexStorage2DEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
glTexStorage2DEXT = ffienumsizeienumsizeisizeiIOV glTexStorage2DEXTFunPtr

glTexStorage2DEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
glTexStorage2DEXTFunPtr = unsafePerformIO (getProcAddress "glTexStorage2DEXT")

{-# NOINLINE glTexStorage2DEXTFunPtr #-}

-- | Usage: @'glTexStorage3DEXT' target levels internalformat width height depth@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTexStorage3D'.


glTexStorage3DEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> m ()
glTexStorage3DEXT = ffienumsizeienumsizeisizeisizeiIOV glTexStorage3DEXTFunPtr

glTexStorage3DEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
glTexStorage3DEXTFunPtr = unsafePerformIO (getProcAddress "glTexStorage3DEXT")

{-# NOINLINE glTexStorage3DEXTFunPtr #-}

pattern GL_ALPHA16F_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA16F_EXT = 0x881C

pattern GL_ALPHA32F_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA32F_EXT = 0x8816

pattern GL_LUMINANCE16F_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16F_EXT = 0x881E

pattern GL_LUMINANCE32F_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE32F_EXT = 0x8818

pattern GL_LUMINANCE_ALPHA16F_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA16F_EXT = 0x881F

pattern GL_LUMINANCE_ALPHA32F_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA32F_EXT = 0x8819

pattern GL_R32F_EXT  :: (Eq a, Num a) => a

pattern GL_R32F_EXT = 0x822E

pattern GL_RG32F_EXT  :: (Eq a, Num a) => a

pattern GL_RG32F_EXT = 0x8230

pattern GL_RGB32F_EXT  :: (Eq a, Num a) => a

pattern GL_RGB32F_EXT = 0x8815

pattern GL_RGBA32F_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA32F_EXT = 0x8814

pattern GL_TEXTURE_IMMUTABLE_FORMAT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_IMMUTABLE_FORMAT_EXT = 0x912F