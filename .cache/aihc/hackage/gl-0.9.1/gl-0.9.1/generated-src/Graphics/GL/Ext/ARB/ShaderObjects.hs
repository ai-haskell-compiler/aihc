-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderObjects (
  -- * Extension Support
    gl_ARB_shader_objects

  -- * GL_ARB_shader_objects
  , glAttachObjectARB
  , glCompileShaderARB
  , glCreateProgramObjectARB
  , glCreateShaderObjectARB
  , glDeleteObjectARB
  , glDetachObjectARB
  , glGetActiveUniformARB
  , glGetAttachedObjectsARB
  , glGetHandleARB
  , glGetInfoLogARB
  , glGetObjectParameterfvARB
  , glGetObjectParameterivARB
  , glGetShaderSourceARB
  , glGetUniformLocationARB
  , glGetUniformfvARB
  , glGetUniformivARB
  , glLinkProgramARB
  , glShaderSourceARB
  , glUniform1fARB
  , glUniform1fvARB
  , glUniform1iARB
  , glUniform1ivARB
  , glUniform2fARB
  , glUniform2fvARB
  , glUniform2iARB
  , glUniform2ivARB
  , glUniform3fARB
  , glUniform3fvARB
  , glUniform3iARB
  , glUniform3ivARB
  , glUniform4fARB
  , glUniform4fvARB
  , glUniform4iARB
  , glUniform4ivARB
  , glUniformMatrix2fvARB
  , glUniformMatrix3fvARB
  , glUniformMatrix4fvARB
  , glUseProgramObjectARB
  , glValidateProgramARB
  , pattern GL_BOOL_ARB
  , pattern GL_BOOL_VEC2_ARB
  , pattern GL_BOOL_VEC3_ARB
  , pattern GL_BOOL_VEC4_ARB
  , pattern GL_FLOAT_MAT2_ARB
  , pattern GL_FLOAT_MAT3_ARB
  , pattern GL_FLOAT_MAT4_ARB
  , pattern GL_FLOAT_VEC2_ARB
  , pattern GL_FLOAT_VEC3_ARB
  , pattern GL_FLOAT_VEC4_ARB
  , pattern GL_INT_VEC2_ARB
  , pattern GL_INT_VEC3_ARB
  , pattern GL_INT_VEC4_ARB
  , pattern GL_OBJECT_ACTIVE_UNIFORMS_ARB
  , pattern GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB
  , pattern GL_OBJECT_ATTACHED_OBJECTS_ARB
  , pattern GL_OBJECT_COMPILE_STATUS_ARB
  , pattern GL_OBJECT_DELETE_STATUS_ARB
  , pattern GL_OBJECT_INFO_LOG_LENGTH_ARB
  , pattern GL_OBJECT_LINK_STATUS_ARB
  , pattern GL_OBJECT_SHADER_SOURCE_LENGTH_ARB
  , pattern GL_OBJECT_SUBTYPE_ARB
  , pattern GL_OBJECT_TYPE_ARB
  , pattern GL_OBJECT_VALIDATE_STATUS_ARB
  , pattern GL_PROGRAM_OBJECT_ARB
  , pattern GL_SAMPLER_1D_ARB
  , pattern GL_SAMPLER_1D_SHADOW_ARB
  , pattern GL_SAMPLER_2D_ARB
  , pattern GL_SAMPLER_2D_RECT_ARB
  , pattern GL_SAMPLER_2D_RECT_SHADOW_ARB
  , pattern GL_SAMPLER_2D_SHADOW_ARB
  , pattern GL_SAMPLER_3D_ARB
  , pattern GL_SAMPLER_CUBE_ARB
  , pattern GL_SHADER_OBJECT_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_objects.txt GL_ARB_shader_objects> extension is available.

gl_ARB_shader_objects :: Bool
gl_ARB_shader_objects = member "GL_ARB_shader_objects" extensions
{-# NOINLINE gl_ARB_shader_objects #-}

-- | Usage: @'glAttachObjectARB' containerObj obj@
--
-- The parameter @containerObj@ is a @handleARB@.
--
-- The parameter @obj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glAttachShader'.


glAttachObjectARB :: MonadIO m => GLhandleARB -> GLhandleARB -> m ()
glAttachObjectARB = ffihandleARBhandleARBIOV glAttachObjectARBFunPtr

glAttachObjectARBFunPtr :: FunPtr (GLhandleARB -> GLhandleARB -> IO ())
glAttachObjectARBFunPtr = unsafePerformIO (getProcAddress "glAttachObjectARB")

{-# NOINLINE glAttachObjectARBFunPtr #-}

-- | Usage: @'glCompileShaderARB' shaderObj@
--
-- The parameter @shaderObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCompileShader'.


glCompileShaderARB :: MonadIO m => GLhandleARB -> m ()
glCompileShaderARB = ffihandleARBIOV glCompileShaderARBFunPtr

glCompileShaderARBFunPtr :: FunPtr (GLhandleARB -> IO ())
glCompileShaderARBFunPtr = unsafePerformIO (getProcAddress "glCompileShaderARB")

{-# NOINLINE glCompileShaderARBFunPtr #-}

-- | Usage: @'glCreateProgramObjectARB'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCreateProgram'.


glCreateProgramObjectARB :: MonadIO m => m GLhandleARB
glCreateProgramObjectARB = ffiIOhandleARB glCreateProgramObjectARBFunPtr

glCreateProgramObjectARBFunPtr :: FunPtr (IO GLhandleARB)
glCreateProgramObjectARBFunPtr = unsafePerformIO (getProcAddress "glCreateProgramObjectARB")

{-# NOINLINE glCreateProgramObjectARBFunPtr #-}

-- | Usage: @'glCreateShaderObjectARB' shaderType@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCreateShader'.


glCreateShaderObjectARB :: MonadIO m => GLenum -> m GLhandleARB
glCreateShaderObjectARB = ffienumIOhandleARB glCreateShaderObjectARBFunPtr

glCreateShaderObjectARBFunPtr :: FunPtr (GLenum -> IO GLhandleARB)
glCreateShaderObjectARBFunPtr = unsafePerformIO (getProcAddress "glCreateShaderObjectARB")

{-# NOINLINE glCreateShaderObjectARBFunPtr #-}

-- | Usage: @'glDeleteObjectARB' obj@
--
-- The parameter @obj@ is a @handleARB@.


glDeleteObjectARB :: MonadIO m => GLhandleARB -> m ()
glDeleteObjectARB = ffihandleARBIOV glDeleteObjectARBFunPtr

glDeleteObjectARBFunPtr :: FunPtr (GLhandleARB -> IO ())
glDeleteObjectARBFunPtr = unsafePerformIO (getProcAddress "glDeleteObjectARB")

{-# NOINLINE glDeleteObjectARBFunPtr #-}

-- | Usage: @'glDetachObjectARB' containerObj attachedObj@
--
-- The parameter @containerObj@ is a @handleARB@.
--
-- The parameter @attachedObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDetachShader'.


glDetachObjectARB :: MonadIO m => GLhandleARB -> GLhandleARB -> m ()
glDetachObjectARB = ffihandleARBhandleARBIOV glDetachObjectARBFunPtr

glDetachObjectARBFunPtr :: FunPtr (GLhandleARB -> GLhandleARB -> IO ())
glDetachObjectARBFunPtr = unsafePerformIO (getProcAddress "glDetachObjectARB")

{-# NOINLINE glDetachObjectARBFunPtr #-}

-- | Usage: @'glGetActiveUniformARB' programObj index maxLength length size type name@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- The length of @length@ should be @1@.
--
-- The length of @size@ should be @1@.
--
-- The length of @type@ should be @1@.
--
-- The length of @name@ should be @maxLength@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetActiveUniform'.


glGetActiveUniformARB :: MonadIO m => GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> m ()
glGetActiveUniformARB = ffihandleARBuintsizeiPtrsizeiPtrintPtrenumPtrcharARBIOV glGetActiveUniformARBFunPtr

glGetActiveUniformARBFunPtr :: FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ())
glGetActiveUniformARBFunPtr = unsafePerformIO (getProcAddress "glGetActiveUniformARB")

{-# NOINLINE glGetActiveUniformARBFunPtr #-}

-- | Usage: @'glGetAttachedObjectsARB' containerObj maxCount count obj@
--
-- The parameter @containerObj@ is a @handleARB@.
--
-- The parameter @obj@ is a @handleARB@.
--
-- The length of @count@ should be @1@.
--
-- The length of @obj@ should be @maxCount@.


glGetAttachedObjectsARB :: MonadIO m => GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> m ()
glGetAttachedObjectsARB = ffihandleARBsizeiPtrsizeiPtrhandleARBIOV glGetAttachedObjectsARBFunPtr

glGetAttachedObjectsARBFunPtr :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> IO ())
glGetAttachedObjectsARBFunPtr = unsafePerformIO (getProcAddress "glGetAttachedObjectsARB")

{-# NOINLINE glGetAttachedObjectsARBFunPtr #-}

-- | Usage: @'glGetHandleARB' pname@


glGetHandleARB :: MonadIO m => GLenum -> m GLhandleARB
glGetHandleARB = ffienumIOhandleARB glGetHandleARBFunPtr

glGetHandleARBFunPtr :: FunPtr (GLenum -> IO GLhandleARB)
glGetHandleARBFunPtr = unsafePerformIO (getProcAddress "glGetHandleARB")

{-# NOINLINE glGetHandleARBFunPtr #-}

-- | Usage: @'glGetInfoLogARB' obj maxLength length infoLog@
--
-- The parameter @obj@ is a @handleARB@.
--
-- The length of @length@ should be @1@.
--
-- The length of @infoLog@ should be @maxLength@.


glGetInfoLogARB :: MonadIO m => GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> m ()
glGetInfoLogARB = ffihandleARBsizeiPtrsizeiPtrcharARBIOV glGetInfoLogARBFunPtr

glGetInfoLogARBFunPtr :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ())
glGetInfoLogARBFunPtr = unsafePerformIO (getProcAddress "glGetInfoLogARB")

{-# NOINLINE glGetInfoLogARBFunPtr #-}

-- | Usage: @'glGetObjectParameterfvARB' obj pname params@
--
-- The parameter @obj@ is a @handleARB@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetObjectParameterfvARB :: MonadIO m => GLhandleARB -> GLenum -> Ptr GLfloat -> m ()
glGetObjectParameterfvARB = ffihandleARBenumPtrfloatIOV glGetObjectParameterfvARBFunPtr

glGetObjectParameterfvARBFunPtr :: FunPtr (GLhandleARB -> GLenum -> Ptr GLfloat -> IO ())
glGetObjectParameterfvARBFunPtr = unsafePerformIO (getProcAddress "glGetObjectParameterfvARB")

{-# NOINLINE glGetObjectParameterfvARBFunPtr #-}

-- | Usage: @'glGetObjectParameterivARB' obj pname params@
--
-- The parameter @obj@ is a @handleARB@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetObjectParameterivARB :: MonadIO m => GLhandleARB -> GLenum -> Ptr GLint -> m ()
glGetObjectParameterivARB = ffihandleARBenumPtrintIOV glGetObjectParameterivARBFunPtr

glGetObjectParameterivARBFunPtr :: FunPtr (GLhandleARB -> GLenum -> Ptr GLint -> IO ())
glGetObjectParameterivARBFunPtr = unsafePerformIO (getProcAddress "glGetObjectParameterivARB")

{-# NOINLINE glGetObjectParameterivARBFunPtr #-}

-- | Usage: @'glGetShaderSourceARB' obj maxLength length source@
--
-- The parameter @obj@ is a @handleARB@.
--
-- The length of @length@ should be @1@.
--
-- The length of @source@ should be @maxLength@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetShaderSource'.


glGetShaderSourceARB :: MonadIO m => GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> m ()
glGetShaderSourceARB = ffihandleARBsizeiPtrsizeiPtrcharARBIOV glGetShaderSourceARBFunPtr

glGetShaderSourceARBFunPtr :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ())
glGetShaderSourceARBFunPtr = unsafePerformIO (getProcAddress "glGetShaderSourceARB")

{-# NOINLINE glGetShaderSourceARBFunPtr #-}

-- | Usage: @'glGetUniformLocationARB' programObj name@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetUniformLocation'.


glGetUniformLocationARB :: MonadIO m => GLhandleARB -> Ptr GLcharARB -> m GLint
glGetUniformLocationARB = ffihandleARBPtrcharARBIOint glGetUniformLocationARBFunPtr

glGetUniformLocationARBFunPtr :: FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint)
glGetUniformLocationARBFunPtr = unsafePerformIO (getProcAddress "glGetUniformLocationARB")

{-# NOINLINE glGetUniformLocationARBFunPtr #-}

-- | Usage: @'glGetUniformfvARB' programObj location params@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- The length of @params@ should be @COMPSIZE(programObj,location)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetUniformfv'.


glGetUniformfvARB :: MonadIO m => GLhandleARB -> GLint -> Ptr GLfloat -> m ()
glGetUniformfvARB = ffihandleARBintPtrfloatIOV glGetUniformfvARBFunPtr

glGetUniformfvARBFunPtr :: FunPtr (GLhandleARB -> GLint -> Ptr GLfloat -> IO ())
glGetUniformfvARBFunPtr = unsafePerformIO (getProcAddress "glGetUniformfvARB")

{-# NOINLINE glGetUniformfvARBFunPtr #-}

-- | Usage: @'glGetUniformivARB' programObj location params@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- The length of @params@ should be @COMPSIZE(programObj,location)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetUniformiv'.


glGetUniformivARB :: MonadIO m => GLhandleARB -> GLint -> Ptr GLint -> m ()
glGetUniformivARB = ffihandleARBintPtrintIOV glGetUniformivARBFunPtr

glGetUniformivARBFunPtr :: FunPtr (GLhandleARB -> GLint -> Ptr GLint -> IO ())
glGetUniformivARBFunPtr = unsafePerformIO (getProcAddress "glGetUniformivARB")

{-# NOINLINE glGetUniformivARBFunPtr #-}

-- | Usage: @'glLinkProgramARB' programObj@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glLinkProgram'.


glLinkProgramARB :: MonadIO m => GLhandleARB -> m ()
glLinkProgramARB = ffihandleARBIOV glLinkProgramARBFunPtr

glLinkProgramARBFunPtr :: FunPtr (GLhandleARB -> IO ())
glLinkProgramARBFunPtr = unsafePerformIO (getProcAddress "glLinkProgramARB")

{-# NOINLINE glLinkProgramARBFunPtr #-}

-- | Usage: @'glShaderSourceARB' shaderObj count string length@
--
-- The parameter @shaderObj@ is a @handleARB@.
--
-- The length of @string@ should be @count@.
--
-- The length of @length@ should be @count@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glShaderSource'.


glShaderSourceARB :: MonadIO m => GLhandleARB -> GLsizei -> Ptr (Ptr GLcharARB) -> Ptr GLint -> m ()
glShaderSourceARB = ffihandleARBsizeiPtrPtrcharARBPtrintIOV glShaderSourceARBFunPtr

glShaderSourceARBFunPtr :: FunPtr (GLhandleARB -> GLsizei -> Ptr (Ptr GLcharARB) -> Ptr GLint -> IO ())
glShaderSourceARBFunPtr = unsafePerformIO (getProcAddress "glShaderSourceARB")

{-# NOINLINE glShaderSourceARBFunPtr #-}

-- | Usage: @'glUniform1fARB' location v0@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform1f'.


glUniform1fARB :: MonadIO m => GLint -> GLfloat -> m ()
glUniform1fARB = ffiintfloatIOV glUniform1fARBFunPtr

glUniform1fARBFunPtr :: FunPtr (GLint -> GLfloat -> IO ())
glUniform1fARBFunPtr = unsafePerformIO (getProcAddress "glUniform1fARB")

{-# NOINLINE glUniform1fARBFunPtr #-}

-- | Usage: @'glUniform1fvARB' location count value@
--
-- The length of @value@ should be @count*1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform1fv'.


glUniform1fvARB :: MonadIO m => GLint -> GLsizei -> Ptr GLfloat -> m ()
glUniform1fvARB = ffiintsizeiPtrfloatIOV glUniform1fvARBFunPtr

glUniform1fvARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
glUniform1fvARBFunPtr = unsafePerformIO (getProcAddress "glUniform1fvARB")

{-# NOINLINE glUniform1fvARBFunPtr #-}

-- | Usage: @'glUniform1iARB' location v0@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform1i'.


glUniform1iARB :: MonadIO m => GLint -> GLint -> m ()
glUniform1iARB = ffiintintIOV glUniform1iARBFunPtr

glUniform1iARBFunPtr :: FunPtr (GLint -> GLint -> IO ())
glUniform1iARBFunPtr = unsafePerformIO (getProcAddress "glUniform1iARB")

{-# NOINLINE glUniform1iARBFunPtr #-}

-- | Usage: @'glUniform1ivARB' location count value@
--
-- The length of @value@ should be @count*1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform1iv'.


glUniform1ivARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint -> m ()
glUniform1ivARB = ffiintsizeiPtrintIOV glUniform1ivARBFunPtr

glUniform1ivARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
glUniform1ivARBFunPtr = unsafePerformIO (getProcAddress "glUniform1ivARB")

{-# NOINLINE glUniform1ivARBFunPtr #-}

-- | Usage: @'glUniform2fARB' location v0 v1@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform2f'.


glUniform2fARB :: MonadIO m => GLint -> GLfloat -> GLfloat -> m ()
glUniform2fARB = ffiintfloatfloatIOV glUniform2fARBFunPtr

glUniform2fARBFunPtr :: FunPtr (GLint -> GLfloat -> GLfloat -> IO ())
glUniform2fARBFunPtr = unsafePerformIO (getProcAddress "glUniform2fARB")

{-# NOINLINE glUniform2fARBFunPtr #-}

-- | Usage: @'glUniform2fvARB' location count value@
--
-- The length of @value@ should be @count*2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform2fv'.


glUniform2fvARB :: MonadIO m => GLint -> GLsizei -> Ptr GLfloat -> m ()
glUniform2fvARB = ffiintsizeiPtrfloatIOV glUniform2fvARBFunPtr

glUniform2fvARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
glUniform2fvARBFunPtr = unsafePerformIO (getProcAddress "glUniform2fvARB")

{-# NOINLINE glUniform2fvARBFunPtr #-}

-- | Usage: @'glUniform2iARB' location v0 v1@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform2i'.


glUniform2iARB :: MonadIO m => GLint -> GLint -> GLint -> m ()
glUniform2iARB = ffiintintintIOV glUniform2iARBFunPtr

glUniform2iARBFunPtr :: FunPtr (GLint -> GLint -> GLint -> IO ())
glUniform2iARBFunPtr = unsafePerformIO (getProcAddress "glUniform2iARB")

{-# NOINLINE glUniform2iARBFunPtr #-}

-- | Usage: @'glUniform2ivARB' location count value@
--
-- The length of @value@ should be @count*2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform2iv'.


glUniform2ivARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint -> m ()
glUniform2ivARB = ffiintsizeiPtrintIOV glUniform2ivARBFunPtr

glUniform2ivARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
glUniform2ivARBFunPtr = unsafePerformIO (getProcAddress "glUniform2ivARB")

{-# NOINLINE glUniform2ivARBFunPtr #-}

-- | Usage: @'glUniform3fARB' location v0 v1 v2@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform3f'.


glUniform3fARB :: MonadIO m => GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
glUniform3fARB = ffiintfloatfloatfloatIOV glUniform3fARBFunPtr

glUniform3fARBFunPtr :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
glUniform3fARBFunPtr = unsafePerformIO (getProcAddress "glUniform3fARB")

{-# NOINLINE glUniform3fARBFunPtr #-}

-- | Usage: @'glUniform3fvARB' location count value@
--
-- The length of @value@ should be @count*3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform3fv'.


glUniform3fvARB :: MonadIO m => GLint -> GLsizei -> Ptr GLfloat -> m ()
glUniform3fvARB = ffiintsizeiPtrfloatIOV glUniform3fvARBFunPtr

glUniform3fvARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
glUniform3fvARBFunPtr = unsafePerformIO (getProcAddress "glUniform3fvARB")

{-# NOINLINE glUniform3fvARBFunPtr #-}

-- | Usage: @'glUniform3iARB' location v0 v1 v2@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform3i'.


glUniform3iARB :: MonadIO m => GLint -> GLint -> GLint -> GLint -> m ()
glUniform3iARB = ffiintintintintIOV glUniform3iARBFunPtr

glUniform3iARBFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
glUniform3iARBFunPtr = unsafePerformIO (getProcAddress "glUniform3iARB")

{-# NOINLINE glUniform3iARBFunPtr #-}

-- | Usage: @'glUniform3ivARB' location count value@
--
-- The length of @value@ should be @count*3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform3iv'.


glUniform3ivARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint -> m ()
glUniform3ivARB = ffiintsizeiPtrintIOV glUniform3ivARBFunPtr

glUniform3ivARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
glUniform3ivARBFunPtr = unsafePerformIO (getProcAddress "glUniform3ivARB")

{-# NOINLINE glUniform3ivARBFunPtr #-}

-- | Usage: @'glUniform4fARB' location v0 v1 v2 v3@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform4f'.


glUniform4fARB :: MonadIO m => GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glUniform4fARB = ffiintfloatfloatfloatfloatIOV glUniform4fARBFunPtr

glUniform4fARBFunPtr :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glUniform4fARBFunPtr = unsafePerformIO (getProcAddress "glUniform4fARB")

{-# NOINLINE glUniform4fARBFunPtr #-}

-- | Usage: @'glUniform4fvARB' location count value@
--
-- The length of @value@ should be @count*4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform4fv'.


glUniform4fvARB :: MonadIO m => GLint -> GLsizei -> Ptr GLfloat -> m ()
glUniform4fvARB = ffiintsizeiPtrfloatIOV glUniform4fvARBFunPtr

glUniform4fvARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
glUniform4fvARBFunPtr = unsafePerformIO (getProcAddress "glUniform4fvARB")

{-# NOINLINE glUniform4fvARBFunPtr #-}

-- | Usage: @'glUniform4iARB' location v0 v1 v2 v3@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform4i'.


glUniform4iARB :: MonadIO m => GLint -> GLint -> GLint -> GLint -> GLint -> m ()
glUniform4iARB = ffiintintintintintIOV glUniform4iARBFunPtr

glUniform4iARBFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
glUniform4iARBFunPtr = unsafePerformIO (getProcAddress "glUniform4iARB")

{-# NOINLINE glUniform4iARBFunPtr #-}

-- | Usage: @'glUniform4ivARB' location count value@
--
-- The length of @value@ should be @count*4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform4iv'.


glUniform4ivARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint -> m ()
glUniform4ivARB = ffiintsizeiPtrintIOV glUniform4ivARBFunPtr

glUniform4ivARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
glUniform4ivARBFunPtr = unsafePerformIO (getProcAddress "glUniform4ivARB")

{-# NOINLINE glUniform4ivARBFunPtr #-}

-- | Usage: @'glUniformMatrix2fvARB' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix2fv'.


glUniformMatrix2fvARB :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix2fvARB = ffiintsizeibooleanPtrfloatIOV glUniformMatrix2fvARBFunPtr

glUniformMatrix2fvARBFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix2fvARBFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix2fvARB")

{-# NOINLINE glUniformMatrix2fvARBFunPtr #-}

-- | Usage: @'glUniformMatrix3fvARB' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*9@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix3fv'.


glUniformMatrix3fvARB :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix3fvARB = ffiintsizeibooleanPtrfloatIOV glUniformMatrix3fvARBFunPtr

glUniformMatrix3fvARBFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix3fvARBFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix3fvARB")

{-# NOINLINE glUniformMatrix3fvARBFunPtr #-}

-- | Usage: @'glUniformMatrix4fvARB' location count transpose value@
--
-- The parameter @transpose@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @value@ should be @count*16@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniformMatrix4fv'.


glUniformMatrix4fvARB :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
glUniformMatrix4fvARB = ffiintsizeibooleanPtrfloatIOV glUniformMatrix4fvARBFunPtr

glUniformMatrix4fvARBFunPtr :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
glUniformMatrix4fvARBFunPtr = unsafePerformIO (getProcAddress "glUniformMatrix4fvARB")

{-# NOINLINE glUniformMatrix4fvARBFunPtr #-}

-- | Usage: @'glUseProgramObjectARB' programObj@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUseProgram'.


glUseProgramObjectARB :: MonadIO m => GLhandleARB -> m ()
glUseProgramObjectARB = ffihandleARBIOV glUseProgramObjectARBFunPtr

glUseProgramObjectARBFunPtr :: FunPtr (GLhandleARB -> IO ())
glUseProgramObjectARBFunPtr = unsafePerformIO (getProcAddress "glUseProgramObjectARB")

{-# NOINLINE glUseProgramObjectARBFunPtr #-}

-- | Usage: @'glValidateProgramARB' programObj@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glValidateProgram'.


glValidateProgramARB :: MonadIO m => GLhandleARB -> m ()
glValidateProgramARB = ffihandleARBIOV glValidateProgramARBFunPtr

glValidateProgramARBFunPtr :: FunPtr (GLhandleARB -> IO ())
glValidateProgramARBFunPtr = unsafePerformIO (getProcAddress "glValidateProgramARB")

{-# NOINLINE glValidateProgramARBFunPtr #-}

pattern GL_BOOL_ARB  :: (Eq a, Num a) => a

pattern GL_BOOL_ARB = 0x8B56

pattern GL_BOOL_VEC2_ARB  :: (Eq a, Num a) => a

pattern GL_BOOL_VEC2_ARB = 0x8B57

pattern GL_BOOL_VEC3_ARB  :: (Eq a, Num a) => a

pattern GL_BOOL_VEC3_ARB = 0x8B58

pattern GL_BOOL_VEC4_ARB  :: (Eq a, Num a) => a

pattern GL_BOOL_VEC4_ARB = 0x8B59

pattern GL_INT_VEC2_ARB  :: (Eq a, Num a) => a

pattern GL_INT_VEC2_ARB = 0x8B53

pattern GL_INT_VEC3_ARB  :: (Eq a, Num a) => a

pattern GL_INT_VEC3_ARB = 0x8B54

pattern GL_INT_VEC4_ARB  :: (Eq a, Num a) => a

pattern GL_INT_VEC4_ARB = 0x8B55

pattern GL_OBJECT_ACTIVE_UNIFORMS_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_ACTIVE_UNIFORMS_ARB = 0x8B86

pattern GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB = 0x8B87

pattern GL_OBJECT_ATTACHED_OBJECTS_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_ATTACHED_OBJECTS_ARB = 0x8B85

pattern GL_OBJECT_COMPILE_STATUS_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_COMPILE_STATUS_ARB = 0x8B81

pattern GL_OBJECT_DELETE_STATUS_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_DELETE_STATUS_ARB = 0x8B80

pattern GL_OBJECT_INFO_LOG_LENGTH_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_INFO_LOG_LENGTH_ARB = 0x8B84

pattern GL_OBJECT_LINK_STATUS_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_LINK_STATUS_ARB = 0x8B82

pattern GL_OBJECT_SHADER_SOURCE_LENGTH_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_SHADER_SOURCE_LENGTH_ARB = 0x8B88

pattern GL_OBJECT_SUBTYPE_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_SUBTYPE_ARB = 0x8B4F

pattern GL_OBJECT_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_TYPE_ARB = 0x8B4E

pattern GL_OBJECT_VALIDATE_STATUS_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_VALIDATE_STATUS_ARB = 0x8B83

pattern GL_PROGRAM_OBJECT_ARB  :: (Eq a, Num a) => a

pattern GL_PROGRAM_OBJECT_ARB = 0x8B40

pattern GL_SAMPLER_1D_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_1D_ARB = 0x8B5D

pattern GL_SAMPLER_1D_SHADOW_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_1D_SHADOW_ARB = 0x8B61

pattern GL_SAMPLER_2D_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_ARB = 0x8B5E

pattern GL_SAMPLER_2D_RECT_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_RECT_ARB = 0x8B63

pattern GL_SAMPLER_2D_RECT_SHADOW_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_RECT_SHADOW_ARB = 0x8B64

pattern GL_SAMPLER_2D_SHADOW_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_SHADOW_ARB = 0x8B62

pattern GL_SAMPLER_3D_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_3D_ARB = 0x8B5F

pattern GL_SAMPLER_CUBE_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_ARB = 0x8B60

pattern GL_SHADER_OBJECT_ARB  :: (Eq a, Num a) => a

pattern GL_SHADER_OBJECT_ARB = 0x8B48