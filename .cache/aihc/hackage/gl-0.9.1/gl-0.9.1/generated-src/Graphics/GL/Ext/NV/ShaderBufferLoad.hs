-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderBufferLoad (
  -- * Extension Support
    gl_NV_shader_buffer_load

  -- * GL_NV_shader_buffer_load
  , glGetBufferParameterui64vNV
  , glGetIntegerui64vNV
  , glGetNamedBufferParameterui64vNV
  , glGetUniformui64vNV
  , glIsBufferResidentNV
  , glIsNamedBufferResidentNV
  , glMakeBufferNonResidentNV
  , glMakeBufferResidentNV
  , glMakeNamedBufferNonResidentNV
  , glMakeNamedBufferResidentNV
  , glProgramUniformui64NV
  , glProgramUniformui64vNV
  , glUniformui64NV
  , glUniformui64vNV
  , pattern GL_BUFFER_GPU_ADDRESS_NV
  , pattern GL_GPU_ADDRESS_NV
  , pattern GL_MAX_SHADER_BUFFER_ADDRESS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_buffer_load.txt GL_NV_shader_buffer_load> extension is available.

gl_NV_shader_buffer_load :: Bool
gl_NV_shader_buffer_load = member "GL_NV_shader_buffer_load" extensions
{-# NOINLINE gl_NV_shader_buffer_load #-}

-- | Usage: @'glGetBufferParameterui64vNV' target pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetBufferParameterui64vNV :: MonadIO m => GLenum -> GLenum -> Ptr GLuint64EXT -> m ()
glGetBufferParameterui64vNV = ffienumenumPtruint64EXTIOV glGetBufferParameterui64vNVFunPtr

glGetBufferParameterui64vNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLuint64EXT -> IO ())
glGetBufferParameterui64vNVFunPtr = unsafePerformIO (getProcAddress "glGetBufferParameterui64vNV")

{-# NOINLINE glGetBufferParameterui64vNVFunPtr #-}

-- | Usage: @'glGetIntegerui64vNV' value result@
--
-- The length of @result@ should be @COMPSIZE(value)@.


glGetIntegerui64vNV :: MonadIO m => GLenum -> Ptr GLuint64EXT -> m ()
glGetIntegerui64vNV = ffienumPtruint64EXTIOV glGetIntegerui64vNVFunPtr

glGetIntegerui64vNVFunPtr :: FunPtr (GLenum -> Ptr GLuint64EXT -> IO ())
glGetIntegerui64vNVFunPtr = unsafePerformIO (getProcAddress "glGetIntegerui64vNV")

{-# NOINLINE glGetIntegerui64vNVFunPtr #-}

-- | Usage: @'glGetNamedBufferParameterui64vNV' buffer pname params@
--
-- The parameter @pname@ is a @VertexBufferObjectParameter@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetNamedBufferParameterui64vNV :: MonadIO m => GLuint -> GLenum -> Ptr GLuint64EXT -> m ()
glGetNamedBufferParameterui64vNV = ffiuintenumPtruint64EXTIOV glGetNamedBufferParameterui64vNVFunPtr

glGetNamedBufferParameterui64vNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
glGetNamedBufferParameterui64vNVFunPtr = unsafePerformIO (getProcAddress "glGetNamedBufferParameterui64vNV")

{-# NOINLINE glGetNamedBufferParameterui64vNVFunPtr #-}

-- | Usage: @'glIsBufferResidentNV' target@


glIsBufferResidentNV :: MonadIO m => GLenum -> m GLboolean
glIsBufferResidentNV = ffienumIOboolean glIsBufferResidentNVFunPtr

glIsBufferResidentNVFunPtr :: FunPtr (GLenum -> IO GLboolean)
glIsBufferResidentNVFunPtr = unsafePerformIO (getProcAddress "glIsBufferResidentNV")

{-# NOINLINE glIsBufferResidentNVFunPtr #-}

-- | Usage: @'glIsNamedBufferResidentNV' buffer@


glIsNamedBufferResidentNV :: MonadIO m => GLuint -> m GLboolean
glIsNamedBufferResidentNV = ffiuintIOboolean glIsNamedBufferResidentNVFunPtr

glIsNamedBufferResidentNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsNamedBufferResidentNVFunPtr = unsafePerformIO (getProcAddress "glIsNamedBufferResidentNV")

{-# NOINLINE glIsNamedBufferResidentNVFunPtr #-}

-- | Usage: @'glMakeBufferNonResidentNV' target@


glMakeBufferNonResidentNV :: MonadIO m => GLenum -> m ()
glMakeBufferNonResidentNV = ffienumIOV glMakeBufferNonResidentNVFunPtr

glMakeBufferNonResidentNVFunPtr :: FunPtr (GLenum -> IO ())
glMakeBufferNonResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeBufferNonResidentNV")

{-# NOINLINE glMakeBufferNonResidentNVFunPtr #-}

-- | Usage: @'glMakeBufferResidentNV' target access@


glMakeBufferResidentNV :: MonadIO m => GLenum -> GLenum -> m ()
glMakeBufferResidentNV = ffienumenumIOV glMakeBufferResidentNVFunPtr

glMakeBufferResidentNVFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glMakeBufferResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeBufferResidentNV")

{-# NOINLINE glMakeBufferResidentNVFunPtr #-}

-- | Usage: @'glMakeNamedBufferNonResidentNV' buffer@


glMakeNamedBufferNonResidentNV :: MonadIO m => GLuint -> m ()
glMakeNamedBufferNonResidentNV = ffiuintIOV glMakeNamedBufferNonResidentNVFunPtr

glMakeNamedBufferNonResidentNVFunPtr :: FunPtr (GLuint -> IO ())
glMakeNamedBufferNonResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeNamedBufferNonResidentNV")

{-# NOINLINE glMakeNamedBufferNonResidentNVFunPtr #-}

-- | Usage: @'glMakeNamedBufferResidentNV' buffer access@


glMakeNamedBufferResidentNV :: MonadIO m => GLuint -> GLenum -> m ()
glMakeNamedBufferResidentNV = ffiuintenumIOV glMakeNamedBufferResidentNVFunPtr

glMakeNamedBufferResidentNVFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glMakeNamedBufferResidentNVFunPtr = unsafePerformIO (getProcAddress "glMakeNamedBufferResidentNV")

{-# NOINLINE glMakeNamedBufferResidentNVFunPtr #-}

-- | Usage: @'glProgramUniformui64NV' program location value@


glProgramUniformui64NV :: MonadIO m => GLuint -> GLint -> GLuint64EXT -> m ()
glProgramUniformui64NV = ffiuintintuint64EXTIOV glProgramUniformui64NVFunPtr

glProgramUniformui64NVFunPtr :: FunPtr (GLuint -> GLint -> GLuint64EXT -> IO ())
glProgramUniformui64NVFunPtr = unsafePerformIO (getProcAddress "glProgramUniformui64NV")

{-# NOINLINE glProgramUniformui64NVFunPtr #-}

-- | Usage: @'glProgramUniformui64vNV' program location count value@
--
-- The length of @value@ should be @count@.


glProgramUniformui64vNV :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> m ()
glProgramUniformui64vNV = ffiuintintsizeiPtruint64EXTIOV glProgramUniformui64vNVFunPtr

glProgramUniformui64vNVFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
glProgramUniformui64vNVFunPtr = unsafePerformIO (getProcAddress "glProgramUniformui64vNV")

{-# NOINLINE glProgramUniformui64vNVFunPtr #-}

-- | Usage: @'glUniformui64NV' location value@


glUniformui64NV :: MonadIO m => GLint -> GLuint64EXT -> m ()
glUniformui64NV = ffiintuint64EXTIOV glUniformui64NVFunPtr

glUniformui64NVFunPtr :: FunPtr (GLint -> GLuint64EXT -> IO ())
glUniformui64NVFunPtr = unsafePerformIO (getProcAddress "glUniformui64NV")

{-# NOINLINE glUniformui64NVFunPtr #-}

-- | Usage: @'glUniformui64vNV' location count value@
--
-- The length of @value@ should be @count*1@.


glUniformui64vNV :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64EXT -> m ()
glUniformui64vNV = ffiintsizeiPtruint64EXTIOV glUniformui64vNVFunPtr

glUniformui64vNVFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
glUniformui64vNVFunPtr = unsafePerformIO (getProcAddress "glUniformui64vNV")

{-# NOINLINE glUniformui64vNVFunPtr #-}

pattern GL_BUFFER_GPU_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_BUFFER_GPU_ADDRESS_NV = 0x8F1D

pattern GL_GPU_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_GPU_ADDRESS_NV = 0x8F34

pattern GL_MAX_SHADER_BUFFER_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_SHADER_BUFFER_ADDRESS_NV = 0x8F35