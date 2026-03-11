-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.GpuProgram4 (
  -- * Extension Support
    gl_NV_gpu_program4

  -- * GL_NV_gpu_program4
  , glGetProgramEnvParameterIivNV
  , glGetProgramEnvParameterIuivNV
  , glGetProgramLocalParameterIivNV
  , glGetProgramLocalParameterIuivNV
  , glProgramEnvParameterI4iNV
  , glProgramEnvParameterI4ivNV
  , glProgramEnvParameterI4uiNV
  , glProgramEnvParameterI4uivNV
  , glProgramEnvParametersI4ivNV
  , glProgramEnvParametersI4uivNV
  , glProgramLocalParameterI4iNV
  , glProgramLocalParameterI4ivNV
  , glProgramLocalParameterI4uiNV
  , glProgramLocalParameterI4uivNV
  , glProgramLocalParametersI4ivNV
  , glProgramLocalParametersI4uivNV
  , pattern GL_MAX_PROGRAM_ATTRIB_COMPONENTS_NV
  , pattern GL_MAX_PROGRAM_GENERIC_ATTRIBS_NV
  , pattern GL_MAX_PROGRAM_GENERIC_RESULTS_NV
  , pattern GL_MAX_PROGRAM_RESULT_COMPONENTS_NV
  , pattern GL_MAX_PROGRAM_TEXEL_OFFSET_NV
  , pattern GL_MIN_PROGRAM_TEXEL_OFFSET_NV
  , pattern GL_PROGRAM_ATTRIB_COMPONENTS_NV
  , pattern GL_PROGRAM_RESULT_COMPONENTS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/gpu_program4.txt GL_NV_gpu_program4> extension is available.

gl_NV_gpu_program4 :: Bool
gl_NV_gpu_program4 = member "GL_NV_gpu_program4" extensions
{-# NOINLINE gl_NV_gpu_program4 #-}

-- | Usage: @'glGetProgramEnvParameterIivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetProgramEnvParameterIivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLint -> m ()
glGetProgramEnvParameterIivNV = ffienumuintPtrintIOV glGetProgramEnvParameterIivNVFunPtr

glGetProgramEnvParameterIivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
glGetProgramEnvParameterIivNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramEnvParameterIivNV")

{-# NOINLINE glGetProgramEnvParameterIivNVFunPtr #-}

-- | Usage: @'glGetProgramEnvParameterIuivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetProgramEnvParameterIuivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLuint -> m ()
glGetProgramEnvParameterIuivNV = ffienumuintPtruintIOV glGetProgramEnvParameterIuivNVFunPtr

glGetProgramEnvParameterIuivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
glGetProgramEnvParameterIuivNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramEnvParameterIuivNV")

{-# NOINLINE glGetProgramEnvParameterIuivNVFunPtr #-}

-- | Usage: @'glGetProgramLocalParameterIivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetProgramLocalParameterIivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLint -> m ()
glGetProgramLocalParameterIivNV = ffienumuintPtrintIOV glGetProgramLocalParameterIivNVFunPtr

glGetProgramLocalParameterIivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
glGetProgramLocalParameterIivNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramLocalParameterIivNV")

{-# NOINLINE glGetProgramLocalParameterIivNVFunPtr #-}

-- | Usage: @'glGetProgramLocalParameterIuivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glGetProgramLocalParameterIuivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLuint -> m ()
glGetProgramLocalParameterIuivNV = ffienumuintPtruintIOV glGetProgramLocalParameterIuivNVFunPtr

glGetProgramLocalParameterIuivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
glGetProgramLocalParameterIuivNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramLocalParameterIuivNV")

{-# NOINLINE glGetProgramLocalParameterIuivNVFunPtr #-}

-- | Usage: @'glProgramEnvParameterI4iNV' target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.GpuProgram4.glProgramEnvParameterI4ivNV'.


glProgramEnvParameterI4iNV :: MonadIO m => GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
glProgramEnvParameterI4iNV = ffienumuintintintintintIOV glProgramEnvParameterI4iNVFunPtr

glProgramEnvParameterI4iNVFunPtr :: FunPtr (GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
glProgramEnvParameterI4iNVFunPtr = unsafePerformIO (getProcAddress "glProgramEnvParameterI4iNV")

{-# NOINLINE glProgramEnvParameterI4iNVFunPtr #-}

-- | Usage: @'glProgramEnvParameterI4ivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glProgramEnvParameterI4ivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLint -> m ()
glProgramEnvParameterI4ivNV = ffienumuintPtrintIOV glProgramEnvParameterI4ivNVFunPtr

glProgramEnvParameterI4ivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
glProgramEnvParameterI4ivNVFunPtr = unsafePerformIO (getProcAddress "glProgramEnvParameterI4ivNV")

{-# NOINLINE glProgramEnvParameterI4ivNVFunPtr #-}

-- | Usage: @'glProgramEnvParameterI4uiNV' target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.GpuProgram4.glProgramEnvParameterI4uivNV'.


glProgramEnvParameterI4uiNV :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glProgramEnvParameterI4uiNV = ffienumuintuintuintuintuintIOV glProgramEnvParameterI4uiNVFunPtr

glProgramEnvParameterI4uiNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glProgramEnvParameterI4uiNVFunPtr = unsafePerformIO (getProcAddress "glProgramEnvParameterI4uiNV")

{-# NOINLINE glProgramEnvParameterI4uiNVFunPtr #-}

-- | Usage: @'glProgramEnvParameterI4uivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glProgramEnvParameterI4uivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLuint -> m ()
glProgramEnvParameterI4uivNV = ffienumuintPtruintIOV glProgramEnvParameterI4uivNVFunPtr

glProgramEnvParameterI4uivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
glProgramEnvParameterI4uivNVFunPtr = unsafePerformIO (getProcAddress "glProgramEnvParameterI4uivNV")

{-# NOINLINE glProgramEnvParameterI4uivNVFunPtr #-}

-- | Usage: @'glProgramEnvParametersI4ivNV' target index count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count*4@.


glProgramEnvParametersI4ivNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLint -> m ()
glProgramEnvParametersI4ivNV = ffienumuintsizeiPtrintIOV glProgramEnvParametersI4ivNVFunPtr

glProgramEnvParametersI4ivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ())
glProgramEnvParametersI4ivNVFunPtr = unsafePerformIO (getProcAddress "glProgramEnvParametersI4ivNV")

{-# NOINLINE glProgramEnvParametersI4ivNVFunPtr #-}

-- | Usage: @'glProgramEnvParametersI4uivNV' target index count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count*4@.


glProgramEnvParametersI4uivNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLuint -> m ()
glProgramEnvParametersI4uivNV = ffienumuintsizeiPtruintIOV glProgramEnvParametersI4uivNVFunPtr

glProgramEnvParametersI4uivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
glProgramEnvParametersI4uivNVFunPtr = unsafePerformIO (getProcAddress "glProgramEnvParametersI4uivNV")

{-# NOINLINE glProgramEnvParametersI4uivNVFunPtr #-}

-- | Usage: @'glProgramLocalParameterI4iNV' target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.GpuProgram4.glProgramLocalParameterI4ivNV'.


glProgramLocalParameterI4iNV :: MonadIO m => GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
glProgramLocalParameterI4iNV = ffienumuintintintintintIOV glProgramLocalParameterI4iNVFunPtr

glProgramLocalParameterI4iNVFunPtr :: FunPtr (GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
glProgramLocalParameterI4iNVFunPtr = unsafePerformIO (getProcAddress "glProgramLocalParameterI4iNV")

{-# NOINLINE glProgramLocalParameterI4iNVFunPtr #-}

-- | Usage: @'glProgramLocalParameterI4ivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glProgramLocalParameterI4ivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLint -> m ()
glProgramLocalParameterI4ivNV = ffienumuintPtrintIOV glProgramLocalParameterI4ivNVFunPtr

glProgramLocalParameterI4ivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
glProgramLocalParameterI4ivNVFunPtr = unsafePerformIO (getProcAddress "glProgramLocalParameterI4ivNV")

{-# NOINLINE glProgramLocalParameterI4ivNVFunPtr #-}

-- | Usage: @'glProgramLocalParameterI4uiNV' target index x y z w@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.GpuProgram4.glProgramLocalParameterI4uivNV'.


glProgramLocalParameterI4uiNV :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glProgramLocalParameterI4uiNV = ffienumuintuintuintuintuintIOV glProgramLocalParameterI4uiNVFunPtr

glProgramLocalParameterI4uiNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glProgramLocalParameterI4uiNVFunPtr = unsafePerformIO (getProcAddress "glProgramLocalParameterI4uiNV")

{-# NOINLINE glProgramLocalParameterI4uiNVFunPtr #-}

-- | Usage: @'glProgramLocalParameterI4uivNV' target index params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @4@.


glProgramLocalParameterI4uivNV :: MonadIO m => GLenum -> GLuint -> Ptr GLuint -> m ()
glProgramLocalParameterI4uivNV = ffienumuintPtruintIOV glProgramLocalParameterI4uivNVFunPtr

glProgramLocalParameterI4uivNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
glProgramLocalParameterI4uivNVFunPtr = unsafePerformIO (getProcAddress "glProgramLocalParameterI4uivNV")

{-# NOINLINE glProgramLocalParameterI4uivNVFunPtr #-}

-- | Usage: @'glProgramLocalParametersI4ivNV' target index count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count*4@.


glProgramLocalParametersI4ivNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLint -> m ()
glProgramLocalParametersI4ivNV = ffienumuintsizeiPtrintIOV glProgramLocalParametersI4ivNVFunPtr

glProgramLocalParametersI4ivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ())
glProgramLocalParametersI4ivNVFunPtr = unsafePerformIO (getProcAddress "glProgramLocalParametersI4ivNV")

{-# NOINLINE glProgramLocalParametersI4ivNVFunPtr #-}

-- | Usage: @'glProgramLocalParametersI4uivNV' target index count params@
--
-- The parameter @target@ is a @ProgramTarget@.
--
-- The length of @params@ should be @count*4@.


glProgramLocalParametersI4uivNV :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLuint -> m ()
glProgramLocalParametersI4uivNV = ffienumuintsizeiPtruintIOV glProgramLocalParametersI4uivNVFunPtr

glProgramLocalParametersI4uivNVFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
glProgramLocalParametersI4uivNVFunPtr = unsafePerformIO (getProcAddress "glProgramLocalParametersI4uivNV")

{-# NOINLINE glProgramLocalParametersI4uivNVFunPtr #-}

pattern GL_MAX_PROGRAM_ATTRIB_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_ATTRIB_COMPONENTS_NV = 0x8908

pattern GL_MAX_PROGRAM_GENERIC_ATTRIBS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_GENERIC_ATTRIBS_NV = 0x8DA5

pattern GL_MAX_PROGRAM_GENERIC_RESULTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_GENERIC_RESULTS_NV = 0x8DA6

pattern GL_MAX_PROGRAM_RESULT_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_RESULT_COMPONENTS_NV = 0x8909

pattern GL_MAX_PROGRAM_TEXEL_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_TEXEL_OFFSET_NV = 0x8905

pattern GL_MIN_PROGRAM_TEXEL_OFFSET_NV  :: (Eq a, Num a) => a

pattern GL_MIN_PROGRAM_TEXEL_OFFSET_NV = 0x8904

pattern GL_PROGRAM_ATTRIB_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_ATTRIB_COMPONENTS_NV = 0x8906

pattern GL_PROGRAM_RESULT_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_RESULT_COMPONENTS_NV = 0x8907