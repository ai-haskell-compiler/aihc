-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.GpuProgramParameters (
  -- * Extension Support
    gl_EXT_gpu_program_parameters

  -- * GL_EXT_gpu_program_parameters
  , glProgramEnvParameters4fvEXT
  , glProgramLocalParameters4fvEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/gpu_program_parameters.txt GL_EXT_gpu_program_parameters> extension is available.

gl_EXT_gpu_program_parameters :: Bool
gl_EXT_gpu_program_parameters = member "GL_EXT_gpu_program_parameters" extensions
{-# NOINLINE gl_EXT_gpu_program_parameters #-}

-- | Usage: @'glProgramEnvParameters4fvEXT' target index count params@
--
-- The parameter @target@ is a @ProgramTargetARB@.
--
-- The length of @params@ should be @count*4@.


glProgramEnvParameters4fvEXT :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glProgramEnvParameters4fvEXT = ffienumuintsizeiPtrfloatIOV glProgramEnvParameters4fvEXTFunPtr

glProgramEnvParameters4fvEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glProgramEnvParameters4fvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramEnvParameters4fvEXT")

{-# NOINLINE glProgramEnvParameters4fvEXTFunPtr #-}

-- | Usage: @'glProgramLocalParameters4fvEXT' target index count params@
--
-- The parameter @target@ is a @ProgramTargetARB@.
--
-- The length of @params@ should be @count*4@.


glProgramLocalParameters4fvEXT :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
glProgramLocalParameters4fvEXT = ffienumuintsizeiPtrfloatIOV glProgramLocalParameters4fvEXTFunPtr

glProgramLocalParameters4fvEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glProgramLocalParameters4fvEXTFunPtr = unsafePerformIO (getProcAddress "glProgramLocalParameters4fvEXT")

{-# NOINLINE glProgramLocalParameters4fvEXTFunPtr #-}