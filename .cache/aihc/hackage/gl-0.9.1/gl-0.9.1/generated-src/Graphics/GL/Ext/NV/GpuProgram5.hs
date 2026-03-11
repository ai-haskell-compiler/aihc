-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.GpuProgram5 (
  -- * Extension Support
    gl_NV_gpu_program5

  -- * GL_NV_gpu_program5
  , glGetProgramSubroutineParameteruivNV
  , glProgramSubroutineParametersuivNV
  , pattern GL_FRAGMENT_PROGRAM_INTERPOLATION_OFFSET_BITS_NV
  , pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET_NV
  , pattern GL_MAX_GEOMETRY_PROGRAM_INVOCATIONS_NV
  , pattern GL_MAX_PROGRAM_SUBROUTINE_NUM_NV
  , pattern GL_MAX_PROGRAM_SUBROUTINE_PARAMETERS_NV
  , pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_NV
  , pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET_NV
  , pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/gpu_program5.txt GL_NV_gpu_program5> extension is available.

gl_NV_gpu_program5 :: Bool
gl_NV_gpu_program5 = member "GL_NV_gpu_program5" extensions
{-# NOINLINE gl_NV_gpu_program5 #-}

-- | Usage: @'glGetProgramSubroutineParameteruivNV' target index param@
--
-- The length of @param@ should be @COMPSIZE(target)@.


glGetProgramSubroutineParameteruivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLuint -> m ()
glGetProgramSubroutineParameteruivNV = ffienumuintPtruintIOV glGetProgramSubroutineParameteruivNVFunPtr

glGetProgramSubroutineParameteruivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
glGetProgramSubroutineParameteruivNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramSubroutineParameteruivNV")

{-# NOINLINE glGetProgramSubroutineParameteruivNVFunPtr #-}

-- | Usage: @'glProgramSubroutineParametersuivNV' target count params@
--
-- The length of @params@ should be @count@.


glProgramSubroutineParametersuivNV :: MonadIO m => GLenum -> GLsizei -> Ptr GLuint -> m ()
glProgramSubroutineParametersuivNV = ffienumsizeiPtruintIOV glProgramSubroutineParametersuivNVFunPtr

glProgramSubroutineParametersuivNVFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
glProgramSubroutineParametersuivNVFunPtr = unsafePerformIO (getProcAddress "glProgramSubroutineParametersuivNV")

{-# NOINLINE glProgramSubroutineParametersuivNVFunPtr #-}

pattern GL_FRAGMENT_PROGRAM_INTERPOLATION_OFFSET_BITS_NV  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_PROGRAM_INTERPOLATION_OFFSET_BITS_NV = 0x8E5D

pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET_NV = 0x8E5C

pattern GL_MAX_GEOMETRY_PROGRAM_INVOCATIONS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_PROGRAM_INVOCATIONS_NV = 0x8E5A

pattern GL_MAX_PROGRAM_SUBROUTINE_NUM_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_SUBROUTINE_NUM_NV = 0x8F45

pattern GL_MAX_PROGRAM_SUBROUTINE_PARAMETERS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_SUBROUTINE_PARAMETERS_NV = 0x8F44

pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_NV = 0x8E5F

pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET_NV = 0x8E5B

pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_NV = 0x8E5E