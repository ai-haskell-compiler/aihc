-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.BindlessTexture (
  -- * Extension Support
    gl_ARB_bindless_texture

  -- * GL_ARB_bindless_texture
  , glGetImageHandleARB
  , glGetTextureHandleARB
  , glGetTextureSamplerHandleARB
  , glGetVertexAttribLui64vARB
  , glIsImageHandleResidentARB
  , glIsTextureHandleResidentARB
  , glMakeImageHandleNonResidentARB
  , glMakeImageHandleResidentARB
  , glMakeTextureHandleNonResidentARB
  , glMakeTextureHandleResidentARB
  , glProgramUniformHandleui64ARB
  , glProgramUniformHandleui64vARB
  , glUniformHandleui64ARB
  , glUniformHandleui64vARB
  , glVertexAttribL1ui64ARB
  , glVertexAttribL1ui64vARB
  , pattern GL_UNSIGNED_INT64_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/bindless_texture.txt GL_ARB_bindless_texture> extension is available.

gl_ARB_bindless_texture :: Bool
gl_ARB_bindless_texture = member "GL_ARB_bindless_texture" extensions
{-# NOINLINE gl_ARB_bindless_texture #-}

-- | Usage: @'glGetImageHandleARB' texture level layered layer format@


glGetImageHandleARB :: MonadIO m => GLuint -> GLint -> GLboolean -> GLint -> GLenum -> m GLuint64
glGetImageHandleARB = ffiuintintbooleanintenumIOuint64 glGetImageHandleARBFunPtr

glGetImageHandleARBFunPtr :: FunPtr (GLuint -> GLint -> GLboolean -> GLint -> GLenum -> IO GLuint64)
glGetImageHandleARBFunPtr = unsafePerformIO (getProcAddress "glGetImageHandleARB")

{-# NOINLINE glGetImageHandleARBFunPtr #-}

-- | Usage: @'glGetTextureHandleARB' texture@


glGetTextureHandleARB :: MonadIO m => GLuint -> m GLuint64
glGetTextureHandleARB = ffiuintIOuint64 glGetTextureHandleARBFunPtr

glGetTextureHandleARBFunPtr :: FunPtr (GLuint -> IO GLuint64)
glGetTextureHandleARBFunPtr = unsafePerformIO (getProcAddress "glGetTextureHandleARB")

{-# NOINLINE glGetTextureHandleARBFunPtr #-}

-- | Usage: @'glGetTextureSamplerHandleARB' texture sampler@


glGetTextureSamplerHandleARB :: MonadIO m => GLuint -> GLuint -> m GLuint64
glGetTextureSamplerHandleARB = ffiuintuintIOuint64 glGetTextureSamplerHandleARBFunPtr

glGetTextureSamplerHandleARBFunPtr :: FunPtr (GLuint -> GLuint -> IO GLuint64)
glGetTextureSamplerHandleARBFunPtr = unsafePerformIO (getProcAddress "glGetTextureSamplerHandleARB")

{-# NOINLINE glGetTextureSamplerHandleARBFunPtr #-}

-- | Usage: @'glGetVertexAttribLui64vARB' index pname params@


glGetVertexAttribLui64vARB :: MonadIO m => GLuint -> GLenum -> Ptr GLuint64EXT -> m ()
glGetVertexAttribLui64vARB = ffiuintenumPtruint64EXTIOV glGetVertexAttribLui64vARBFunPtr

glGetVertexAttribLui64vARBFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
glGetVertexAttribLui64vARBFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribLui64vARB")

{-# NOINLINE glGetVertexAttribLui64vARBFunPtr #-}

-- | Usage: @'glIsImageHandleResidentARB' handle@


glIsImageHandleResidentARB :: MonadIO m => GLuint64 -> m GLboolean
glIsImageHandleResidentARB = ffiuint64IOboolean glIsImageHandleResidentARBFunPtr

glIsImageHandleResidentARBFunPtr :: FunPtr (GLuint64 -> IO GLboolean)
glIsImageHandleResidentARBFunPtr = unsafePerformIO (getProcAddress "glIsImageHandleResidentARB")

{-# NOINLINE glIsImageHandleResidentARBFunPtr #-}

-- | Usage: @'glIsTextureHandleResidentARB' handle@


glIsTextureHandleResidentARB :: MonadIO m => GLuint64 -> m GLboolean
glIsTextureHandleResidentARB = ffiuint64IOboolean glIsTextureHandleResidentARBFunPtr

glIsTextureHandleResidentARBFunPtr :: FunPtr (GLuint64 -> IO GLboolean)
glIsTextureHandleResidentARBFunPtr = unsafePerformIO (getProcAddress "glIsTextureHandleResidentARB")

{-# NOINLINE glIsTextureHandleResidentARBFunPtr #-}

-- | Usage: @'glMakeImageHandleNonResidentARB' handle@


glMakeImageHandleNonResidentARB :: MonadIO m => GLuint64 -> m ()
glMakeImageHandleNonResidentARB = ffiuint64IOV glMakeImageHandleNonResidentARBFunPtr

glMakeImageHandleNonResidentARBFunPtr :: FunPtr (GLuint64 -> IO ())
glMakeImageHandleNonResidentARBFunPtr = unsafePerformIO (getProcAddress "glMakeImageHandleNonResidentARB")

{-# NOINLINE glMakeImageHandleNonResidentARBFunPtr #-}

-- | Usage: @'glMakeImageHandleResidentARB' handle access@


glMakeImageHandleResidentARB :: MonadIO m => GLuint64 -> GLenum -> m ()
glMakeImageHandleResidentARB = ffiuint64enumIOV glMakeImageHandleResidentARBFunPtr

glMakeImageHandleResidentARBFunPtr :: FunPtr (GLuint64 -> GLenum -> IO ())
glMakeImageHandleResidentARBFunPtr = unsafePerformIO (getProcAddress "glMakeImageHandleResidentARB")

{-# NOINLINE glMakeImageHandleResidentARBFunPtr #-}

-- | Usage: @'glMakeTextureHandleNonResidentARB' handle@


glMakeTextureHandleNonResidentARB :: MonadIO m => GLuint64 -> m ()
glMakeTextureHandleNonResidentARB = ffiuint64IOV glMakeTextureHandleNonResidentARBFunPtr

glMakeTextureHandleNonResidentARBFunPtr :: FunPtr (GLuint64 -> IO ())
glMakeTextureHandleNonResidentARBFunPtr = unsafePerformIO (getProcAddress "glMakeTextureHandleNonResidentARB")

{-# NOINLINE glMakeTextureHandleNonResidentARBFunPtr #-}

-- | Usage: @'glMakeTextureHandleResidentARB' handle@


glMakeTextureHandleResidentARB :: MonadIO m => GLuint64 -> m ()
glMakeTextureHandleResidentARB = ffiuint64IOV glMakeTextureHandleResidentARBFunPtr

glMakeTextureHandleResidentARBFunPtr :: FunPtr (GLuint64 -> IO ())
glMakeTextureHandleResidentARBFunPtr = unsafePerformIO (getProcAddress "glMakeTextureHandleResidentARB")

{-# NOINLINE glMakeTextureHandleResidentARBFunPtr #-}

-- | Usage: @'glProgramUniformHandleui64ARB' program location value@


glProgramUniformHandleui64ARB :: MonadIO m => GLuint -> GLint -> GLuint64 -> m ()
glProgramUniformHandleui64ARB = ffiuintintuint64IOV glProgramUniformHandleui64ARBFunPtr

glProgramUniformHandleui64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
glProgramUniformHandleui64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniformHandleui64ARB")

{-# NOINLINE glProgramUniformHandleui64ARBFunPtr #-}

-- | Usage: @'glProgramUniformHandleui64vARB' program location count values@
--
-- The length of @values@ should be @count@.


glProgramUniformHandleui64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glProgramUniformHandleui64vARB = ffiuintintsizeiPtruint64IOV glProgramUniformHandleui64vARBFunPtr

glProgramUniformHandleui64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glProgramUniformHandleui64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniformHandleui64vARB")

{-# NOINLINE glProgramUniformHandleui64vARBFunPtr #-}

-- | Usage: @'glUniformHandleui64ARB' location value@


glUniformHandleui64ARB :: MonadIO m => GLint -> GLuint64 -> m ()
glUniformHandleui64ARB = ffiintuint64IOV glUniformHandleui64ARBFunPtr

glUniformHandleui64ARBFunPtr :: FunPtr (GLint -> GLuint64 -> IO ())
glUniformHandleui64ARBFunPtr = unsafePerformIO (getProcAddress "glUniformHandleui64ARB")

{-# NOINLINE glUniformHandleui64ARBFunPtr #-}

-- | Usage: @'glUniformHandleui64vARB' location count value@
--
-- The length of @value@ should be @count@.


glUniformHandleui64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64 -> m ()
glUniformHandleui64vARB = ffiintsizeiPtruint64IOV glUniformHandleui64vARBFunPtr

glUniformHandleui64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glUniformHandleui64vARBFunPtr = unsafePerformIO (getProcAddress "glUniformHandleui64vARB")

{-# NOINLINE glUniformHandleui64vARBFunPtr #-}

-- | Usage: @'glVertexAttribL1ui64ARB' index x@


glVertexAttribL1ui64ARB :: MonadIO m => GLuint -> GLuint64EXT -> m ()
glVertexAttribL1ui64ARB = ffiuintuint64EXTIOV glVertexAttribL1ui64ARBFunPtr

glVertexAttribL1ui64ARBFunPtr :: FunPtr (GLuint -> GLuint64EXT -> IO ())
glVertexAttribL1ui64ARBFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1ui64ARB")

{-# NOINLINE glVertexAttribL1ui64ARBFunPtr #-}

-- | Usage: @'glVertexAttribL1ui64vARB' index v@


glVertexAttribL1ui64vARB :: MonadIO m => GLuint -> Ptr GLuint64EXT -> m ()
glVertexAttribL1ui64vARB = ffiuintPtruint64EXTIOV glVertexAttribL1ui64vARBFunPtr

glVertexAttribL1ui64vARBFunPtr :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
glVertexAttribL1ui64vARBFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1ui64vARB")

{-# NOINLINE glVertexAttribL1ui64vARBFunPtr #-}