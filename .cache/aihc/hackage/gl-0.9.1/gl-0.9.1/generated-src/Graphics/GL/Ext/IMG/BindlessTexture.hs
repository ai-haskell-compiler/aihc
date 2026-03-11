-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.BindlessTexture (
  -- * Extension Support
    gl_IMG_bindless_texture

  -- * GL_IMG_bindless_texture
  , glGetTextureHandleIMG
  , glGetTextureSamplerHandleIMG
  , glProgramUniformHandleui64IMG
  , glProgramUniformHandleui64vIMG
  , glUniformHandleui64IMG
  , glUniformHandleui64vIMG
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/IMG/IMG_bindless_texture.txt GL_IMG_bindless_texture> extension is available.

gl_IMG_bindless_texture :: Bool
gl_IMG_bindless_texture = member "GL_IMG_bindless_texture" extensions
{-# NOINLINE gl_IMG_bindless_texture #-}

-- | Usage: @'glGetTextureHandleIMG' texture@
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.BindlessTexture.glGetTextureHandleARB'.


glGetTextureHandleIMG :: MonadIO m => GLuint -> m GLuint64
glGetTextureHandleIMG = ffiuintIOuint64 glGetTextureHandleIMGFunPtr

glGetTextureHandleIMGFunPtr :: FunPtr (GLuint -> IO GLuint64)
glGetTextureHandleIMGFunPtr = unsafePerformIO (getProcAddress "glGetTextureHandleIMG")

{-# NOINLINE glGetTextureHandleIMGFunPtr #-}

-- | Usage: @'glGetTextureSamplerHandleIMG' texture sampler@
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.BindlessTexture.glGetTextureSamplerHandleARB'.


glGetTextureSamplerHandleIMG :: MonadIO m => GLuint -> GLuint -> m GLuint64
glGetTextureSamplerHandleIMG = ffiuintuintIOuint64 glGetTextureSamplerHandleIMGFunPtr

glGetTextureSamplerHandleIMGFunPtr :: FunPtr (GLuint -> GLuint -> IO GLuint64)
glGetTextureSamplerHandleIMGFunPtr = unsafePerformIO (getProcAddress "glGetTextureSamplerHandleIMG")

{-# NOINLINE glGetTextureSamplerHandleIMGFunPtr #-}

-- | Usage: @'glProgramUniformHandleui64IMG' program location value@
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.BindlessTexture.glProgramUniformHandleui64ARB'.


glProgramUniformHandleui64IMG :: MonadIO m => GLuint -> GLint -> GLuint64 -> m ()
glProgramUniformHandleui64IMG = ffiuintintuint64IOV glProgramUniformHandleui64IMGFunPtr

glProgramUniformHandleui64IMGFunPtr :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
glProgramUniformHandleui64IMGFunPtr = unsafePerformIO (getProcAddress "glProgramUniformHandleui64IMG")

{-# NOINLINE glProgramUniformHandleui64IMGFunPtr #-}

-- | Usage: @'glProgramUniformHandleui64vIMG' program location count values@
--
-- The length of @values@ should be @count@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.BindlessTexture.glProgramUniformHandleui64vARB'.


glProgramUniformHandleui64vIMG :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glProgramUniformHandleui64vIMG = ffiuintintsizeiPtruint64IOV glProgramUniformHandleui64vIMGFunPtr

glProgramUniformHandleui64vIMGFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glProgramUniformHandleui64vIMGFunPtr = unsafePerformIO (getProcAddress "glProgramUniformHandleui64vIMG")

{-# NOINLINE glProgramUniformHandleui64vIMGFunPtr #-}

-- | Usage: @'glUniformHandleui64IMG' location value@
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.BindlessTexture.glUniformHandleui64ARB'.


glUniformHandleui64IMG :: MonadIO m => GLint -> GLuint64 -> m ()
glUniformHandleui64IMG = ffiintuint64IOV glUniformHandleui64IMGFunPtr

glUniformHandleui64IMGFunPtr :: FunPtr (GLint -> GLuint64 -> IO ())
glUniformHandleui64IMGFunPtr = unsafePerformIO (getProcAddress "glUniformHandleui64IMG")

{-# NOINLINE glUniformHandleui64IMGFunPtr #-}

-- | Usage: @'glUniformHandleui64vIMG' location count value@
--
-- The length of @value@ should be @count@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.BindlessTexture.glUniformHandleui64vARB'.


glUniformHandleui64vIMG :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64 -> m ()
glUniformHandleui64vIMG = ffiintsizeiPtruint64IOV glUniformHandleui64vIMGFunPtr

glUniformHandleui64vIMGFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glUniformHandleui64vIMGFunPtr = unsafePerformIO (getProcAddress "glUniformHandleui64vIMG")

{-# NOINLINE glUniformHandleui64vIMGFunPtr #-}