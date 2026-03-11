-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ParameterBufferObject (
  -- * Extension Support
    gl_NV_parameter_buffer_object

  -- * GL_NV_parameter_buffer_object
  , glProgramBufferParametersIivNV
  , glProgramBufferParametersIuivNV
  , glProgramBufferParametersfvNV
  , pattern GL_FRAGMENT_PROGRAM_PARAMETER_BUFFER_NV
  , pattern GL_GEOMETRY_PROGRAM_PARAMETER_BUFFER_NV
  , pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_BINDINGS_NV
  , pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_SIZE_NV
  , pattern GL_VERTEX_PROGRAM_PARAMETER_BUFFER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/parameter_buffer_object.txt GL_NV_parameter_buffer_object> extension is available.

gl_NV_parameter_buffer_object :: Bool
gl_NV_parameter_buffer_object = member "GL_NV_parameter_buffer_object" extensions
{-# NOINLINE gl_NV_parameter_buffer_object #-}

-- | Usage: @'glProgramBufferParametersIivNV' target bindingIndex wordIndex count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count@.


glProgramBufferParametersIivNV :: MonadIO m => GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLint -> m ()
glProgramBufferParametersIivNV = ffienumuintuintsizeiPtrintIOV glProgramBufferParametersIivNVFunPtr

glProgramBufferParametersIivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLint -> IO ())
glProgramBufferParametersIivNVFunPtr = unsafePerformIO (getProcAddress "glProgramBufferParametersIivNV")

{-# NOINLINE glProgramBufferParametersIivNVFunPtr #-}

-- | Usage: @'glProgramBufferParametersIuivNV' target bindingIndex wordIndex count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count@.


glProgramBufferParametersIuivNV :: MonadIO m => GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> m ()
glProgramBufferParametersIuivNV = ffienumuintuintsizeiPtruintIOV glProgramBufferParametersIuivNVFunPtr

glProgramBufferParametersIuivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
glProgramBufferParametersIuivNVFunPtr = unsafePerformIO (getProcAddress "glProgramBufferParametersIuivNV")

{-# NOINLINE glProgramBufferParametersIuivNVFunPtr #-}

-- | Usage: @'glProgramBufferParametersfvNV' target bindingIndex wordIndex count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count@.


glProgramBufferParametersfvNV :: MonadIO m => GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glProgramBufferParametersfvNV = ffienumuintuintsizeiPtrfloatIOV glProgramBufferParametersfvNVFunPtr

glProgramBufferParametersfvNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glProgramBufferParametersfvNVFunPtr = unsafePerformIO (getProcAddress "glProgramBufferParametersfvNV")

{-# NOINLINE glProgramBufferParametersfvNVFunPtr #-}

pattern GL_FRAGMENT_PROGRAM_PARAMETER_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_PROGRAM_PARAMETER_BUFFER_NV = 0x8DA4

pattern GL_GEOMETRY_PROGRAM_PARAMETER_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_PROGRAM_PARAMETER_BUFFER_NV = 0x8DA3

pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_BINDINGS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_BINDINGS_NV = 0x8DA0

pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_SIZE_NV = 0x8DA1

pattern GL_VERTEX_PROGRAM_PARAMETER_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_PROGRAM_PARAMETER_BUFFER_NV = 0x8DA2