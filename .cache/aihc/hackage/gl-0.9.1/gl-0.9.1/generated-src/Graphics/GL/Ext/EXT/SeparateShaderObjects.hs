-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SeparateShaderObjects (
  -- * Extension Support
    gl_EXT_separate_shader_objects

  -- * GL_EXT_separate_shader_objects
  , glActiveProgramEXT
  , glActiveShaderProgramEXT
  , glBindProgramPipelineEXT
  , glCreateShaderProgramEXT
  , glCreateShaderProgramvEXT
  , glDeleteProgramPipelinesEXT
  , glGenProgramPipelinesEXT
  , glGetProgramPipelineInfoLogEXT
  , glGetProgramPipelineivEXT
  , glIsProgramPipelineEXT
  , glProgramParameteriEXT
  , glProgramUniform1fEXT
  , glProgramUniform1fvEXT
  , glProgramUniform1iEXT
  , glProgramUniform1ivEXT
  , glProgramUniform1uiEXT
  , glProgramUniform1uivEXT
  , glProgramUniform2fEXT
  , glProgramUniform2fvEXT
  , glProgramUniform2iEXT
  , glProgramUniform2ivEXT
  , glProgramUniform2uiEXT
  , glProgramUniform2uivEXT
  , glProgramUniform3fEXT
  , glProgramUniform3fvEXT
  , glProgramUniform3iEXT
  , glProgramUniform3ivEXT
  , glProgramUniform3uiEXT
  , glProgramUniform3uivEXT
  , glProgramUniform4fEXT
  , glProgramUniform4fvEXT
  , glProgramUniform4iEXT
  , glProgramUniform4ivEXT
  , glProgramUniform4uiEXT
  , glProgramUniform4uivEXT
  , glProgramUniformMatrix2fvEXT
  , glProgramUniformMatrix2x3fvEXT
  , glProgramUniformMatrix2x4fvEXT
  , glProgramUniformMatrix3fvEXT
  , glProgramUniformMatrix3x2fvEXT
  , glProgramUniformMatrix3x4fvEXT
  , glProgramUniformMatrix4fvEXT
  , glProgramUniformMatrix4x2fvEXT
  , glProgramUniformMatrix4x3fvEXT
  , glUseProgramStagesEXT
  , glUseShaderProgramEXT
  , glValidateProgramPipelineEXT
  , pattern GL_ACTIVE_PROGRAM_EXT
  , pattern GL_ALL_SHADER_BITS_EXT
  , pattern GL_FRAGMENT_SHADER_BIT_EXT
  , pattern GL_PROGRAM_PIPELINE_BINDING_EXT
  , pattern GL_PROGRAM_SEPARABLE_EXT
  , pattern GL_VERTEX_SHADER_BIT_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_separate_shader_objects.txt GL_EXT_separate_shader_objects> extension is available.

gl_EXT_separate_shader_objects :: Bool
gl_EXT_separate_shader_objects = member "GL_EXT_separate_shader_objects" extensions
{-# NOINLINE gl_EXT_separate_shader_objects #-}

-- | Usage: @'glActiveProgramEXT' program@


glActiveProgramEXT :: MonadIO m => GLuint -> m ()
glActiveProgramEXT = ffiuintIOV glActiveProgramEXTFunPtr

glActiveProgramEXTFunPtr :: FunPtr (GLuint -> IO ())
glActiveProgramEXTFunPtr = unsafePerformIO (getProcAddress "glActiveProgramEXT")

{-# NOINLINE glActiveProgramEXTFunPtr #-}

-- | Usage: @'glActiveShaderProgramEXT' pipeline program@


glActiveShaderProgramEXT :: MonadIO m => GLuint -> GLuint -> m ()
glActiveShaderProgramEXT = ffiuintuintIOV glActiveShaderProgramEXTFunPtr

glActiveShaderProgramEXTFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glActiveShaderProgramEXTFunPtr = unsafePerformIO (getProcAddress "glActiveShaderProgramEXT")

{-# NOINLINE glActiveShaderProgramEXTFunPtr #-}

-- | Usage: @'glBindProgramPipelineEXT' pipeline@


glBindProgramPipelineEXT :: MonadIO m => GLuint -> m ()
glBindProgramPipelineEXT = ffiuintIOV glBindProgramPipelineEXTFunPtr

glBindProgramPipelineEXTFunPtr :: FunPtr (GLuint -> IO ())
glBindProgramPipelineEXTFunPtr = unsafePerformIO (getProcAddress "glBindProgramPipelineEXT")

{-# NOINLINE glBindProgramPipelineEXTFunPtr #-}

-- | Usage: @'glCreateShaderProgramEXT' type string@


glCreateShaderProgramEXT :: MonadIO m => GLenum -> Ptr GLchar -> m GLuint
glCreateShaderProgramEXT = ffienumPtrcharIOuint glCreateShaderProgramEXTFunPtr

glCreateShaderProgramEXTFunPtr :: FunPtr (GLenum -> Ptr GLchar -> IO GLuint)
glCreateShaderProgramEXTFunPtr = unsafePerformIO (getProcAddress "glCreateShaderProgramEXT")

{-# NOINLINE glCreateShaderProgramEXTFunPtr #-}

-- | Usage: @'glCreateShaderProgramvEXT' type count strings@
--
-- The length of @strings@ should be @count@.


glCreateShaderProgramvEXT :: MonadIO m => GLenum -> GLsizei -> Ptr (Ptr GLchar) -> m GLuint
glCreateShaderProgramvEXT = ffienumsizeiPtrPtrcharIOuint glCreateShaderProgramvEXTFunPtr

glCreateShaderProgramvEXTFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint)
glCreateShaderProgramvEXTFunPtr = unsafePerformIO (getProcAddress "glCreateShaderProgramvEXT")

{-# NOINLINE glCreateShaderProgramvEXTFunPtr #-}

-- | Usage: @'glDeleteProgramPipelinesEXT' n pipelines@
--
-- The length of @pipelines@ should be @n@.


glDeleteProgramPipelinesEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteProgramPipelinesEXT = ffisizeiPtruintIOV glDeleteProgramPipelinesEXTFunPtr

glDeleteProgramPipelinesEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteProgramPipelinesEXTFunPtr = unsafePerformIO (getProcAddress "glDeleteProgramPipelinesEXT")

{-# NOINLINE glDeleteProgramPipelinesEXTFunPtr #-}

-- | Usage: @'glGenProgramPipelinesEXT' n pipelines@
--
-- The length of @pipelines@ should be @n@.


glGenProgramPipelinesEXT :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenProgramPipelinesEXT = ffisizeiPtruintIOV glGenProgramPipelinesEXTFunPtr

glGenProgramPipelinesEXTFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenProgramPipelinesEXTFunPtr = unsafePerformIO (getProcAddress "glGenProgramPipelinesEXT")

{-# NOINLINE glGenProgramPipelinesEXTFunPtr #-}

-- | Usage: @'glGetProgramPipelineInfoLogEXT' pipeline bufSize length infoLog@
--
-- The length of @length@ should be @1@.
--
-- The length of @infoLog@ should be @bufSize@.


glGetProgramPipelineInfoLogEXT :: MonadIO m => GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetProgramPipelineInfoLogEXT = ffiuintsizeiPtrsizeiPtrcharIOV glGetProgramPipelineInfoLogEXTFunPtr

glGetProgramPipelineInfoLogEXTFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetProgramPipelineInfoLogEXTFunPtr = unsafePerformIO (getProcAddress "glGetProgramPipelineInfoLogEXT")

{-# NOINLINE glGetProgramPipelineInfoLogEXTFunPtr #-}

-- | Usage: @'glGetProgramPipelineivEXT' pipeline pname params@


glGetProgramPipelineivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetProgramPipelineivEXT = ffiuintenumPtrintIOV glGetProgramPipelineivEXTFunPtr

glGetProgramPipelineivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetProgramPipelineivEXTFunPtr = unsafePerformIO (getProcAddress "glGetProgramPipelineivEXT")

{-# NOINLINE glGetProgramPipelineivEXTFunPtr #-}

-- | Usage: @'glIsProgramPipelineEXT' pipeline@


glIsProgramPipelineEXT :: MonadIO m => GLuint -> m GLboolean
glIsProgramPipelineEXT = ffiuintIOboolean glIsProgramPipelineEXTFunPtr

glIsProgramPipelineEXTFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsProgramPipelineEXTFunPtr = unsafePerformIO (getProcAddress "glIsProgramPipelineEXT")

{-# NOINLINE glIsProgramPipelineEXTFunPtr #-}

-- | Usage: @'glUseProgramStagesEXT' pipeline stages program@


glUseProgramStagesEXT :: MonadIO m => GLuint -> GLbitfield -> GLuint -> m ()
glUseProgramStagesEXT = ffiuintbitfielduintIOV glUseProgramStagesEXTFunPtr

glUseProgramStagesEXTFunPtr :: FunPtr (GLuint -> GLbitfield -> GLuint -> IO ())
glUseProgramStagesEXTFunPtr = unsafePerformIO (getProcAddress "glUseProgramStagesEXT")

{-# NOINLINE glUseProgramStagesEXTFunPtr #-}

-- | Usage: @'glUseShaderProgramEXT' type program@


glUseShaderProgramEXT :: MonadIO m => GLenum -> GLuint -> m ()
glUseShaderProgramEXT = ffienumuintIOV glUseShaderProgramEXTFunPtr

glUseShaderProgramEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glUseShaderProgramEXTFunPtr = unsafePerformIO (getProcAddress "glUseShaderProgramEXT")

{-# NOINLINE glUseShaderProgramEXTFunPtr #-}

-- | Usage: @'glValidateProgramPipelineEXT' pipeline@


glValidateProgramPipelineEXT :: MonadIO m => GLuint -> m ()
glValidateProgramPipelineEXT = ffiuintIOV glValidateProgramPipelineEXTFunPtr

glValidateProgramPipelineEXTFunPtr :: FunPtr (GLuint -> IO ())
glValidateProgramPipelineEXTFunPtr = unsafePerformIO (getProcAddress "glValidateProgramPipelineEXT")

{-# NOINLINE glValidateProgramPipelineEXTFunPtr #-}

pattern GL_ACTIVE_PROGRAM_EXT  :: (Eq a, Num a) => a

pattern GL_ACTIVE_PROGRAM_EXT = 0x8B8D

pattern GL_ALL_SHADER_BITS_EXT  :: (Eq a, Num a) => a

pattern GL_ALL_SHADER_BITS_EXT = 0xFFFFFFFF

pattern GL_FRAGMENT_SHADER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_BIT_EXT = 0x00000002

pattern GL_PROGRAM_PIPELINE_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_PROGRAM_PIPELINE_BINDING_EXT = 0x825A

pattern GL_PROGRAM_SEPARABLE_EXT  :: (Eq a, Num a) => a

pattern GL_PROGRAM_SEPARABLE_EXT = 0x8258

pattern GL_VERTEX_SHADER_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_BIT_EXT = 0x00000001