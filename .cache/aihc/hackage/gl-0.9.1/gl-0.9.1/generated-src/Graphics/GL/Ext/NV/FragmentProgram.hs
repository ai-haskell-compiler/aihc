-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FragmentProgram (
  -- * Extension Support
    gl_NV_fragment_program

  -- * GL_NV_fragment_program
  , glGetProgramNamedParameterdvNV
  , glGetProgramNamedParameterfvNV
  , glProgramNamedParameter4dNV
  , glProgramNamedParameter4dvNV
  , glProgramNamedParameter4fNV
  , glProgramNamedParameter4fvNV
  , pattern GL_FRAGMENT_PROGRAM_BINDING_NV
  , pattern GL_FRAGMENT_PROGRAM_NV
  , pattern GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV
  , pattern GL_MAX_TEXTURE_COORDS_NV
  , pattern GL_MAX_TEXTURE_IMAGE_UNITS_NV
  , pattern GL_PROGRAM_ERROR_STRING_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/fragment_program.txt GL_NV_fragment_program> extension is available.

gl_NV_fragment_program :: Bool
gl_NV_fragment_program = member "GL_NV_fragment_program" extensions
{-# NOINLINE gl_NV_fragment_program #-}

-- | Usage: @'glGetProgramNamedParameterdvNV' id len name params@
--
-- The length of @name@ should be @1@.
--
-- The length of @params@ should be @4@.


glGetProgramNamedParameterdvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> m ()
glGetProgramNamedParameterdvNV = ffiuintsizeiPtrubytePtrdoubleIOV glGetProgramNamedParameterdvNVFunPtr

glGetProgramNamedParameterdvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ())
glGetProgramNamedParameterdvNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramNamedParameterdvNV")

{-# NOINLINE glGetProgramNamedParameterdvNVFunPtr #-}

-- | Usage: @'glGetProgramNamedParameterfvNV' id len name params@
--
-- The length of @name@ should be @1@.
--
-- The length of @params@ should be @4@.


glGetProgramNamedParameterfvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> m ()
glGetProgramNamedParameterfvNV = ffiuintsizeiPtrubytePtrfloatIOV glGetProgramNamedParameterfvNVFunPtr

glGetProgramNamedParameterfvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ())
glGetProgramNamedParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetProgramNamedParameterfvNV")

{-# NOINLINE glGetProgramNamedParameterfvNVFunPtr #-}

-- | Usage: @'glProgramNamedParameter4dNV' id len name x y z w@
--
-- The length of @name@ should be @1@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.FragmentProgram.glProgramNamedParameter4dvNV'.


glProgramNamedParameter4dNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glProgramNamedParameter4dNV = ffiuintsizeiPtrubytedoubledoubledoubledoubleIOV glProgramNamedParameter4dNVFunPtr

glProgramNamedParameter4dNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glProgramNamedParameter4dNVFunPtr = unsafePerformIO (getProcAddress "glProgramNamedParameter4dNV")

{-# NOINLINE glProgramNamedParameter4dNVFunPtr #-}

-- | Usage: @'glProgramNamedParameter4dvNV' id len name v@
--
-- The length of @name@ should be @1@.
--
-- The length of @v@ should be @4@.


glProgramNamedParameter4dvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> m ()
glProgramNamedParameter4dvNV = ffiuintsizeiPtrubytePtrdoubleIOV glProgramNamedParameter4dvNVFunPtr

glProgramNamedParameter4dvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ())
glProgramNamedParameter4dvNVFunPtr = unsafePerformIO (getProcAddress "glProgramNamedParameter4dvNV")

{-# NOINLINE glProgramNamedParameter4dvNVFunPtr #-}

-- | Usage: @'glProgramNamedParameter4fNV' id len name x y z w@
--
-- The length of @name@ should be @1@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.FragmentProgram.glProgramNamedParameter4fvNV'.


glProgramNamedParameter4fNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glProgramNamedParameter4fNV = ffiuintsizeiPtrubytefloatfloatfloatfloatIOV glProgramNamedParameter4fNVFunPtr

glProgramNamedParameter4fNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glProgramNamedParameter4fNVFunPtr = unsafePerformIO (getProcAddress "glProgramNamedParameter4fNV")

{-# NOINLINE glProgramNamedParameter4fNVFunPtr #-}

-- | Usage: @'glProgramNamedParameter4fvNV' id len name v@
--
-- The length of @name@ should be @1@.
--
-- The length of @v@ should be @4@.


glProgramNamedParameter4fvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> m ()
glProgramNamedParameter4fvNV = ffiuintsizeiPtrubytePtrfloatIOV glProgramNamedParameter4fvNVFunPtr

glProgramNamedParameter4fvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ())
glProgramNamedParameter4fvNVFunPtr = unsafePerformIO (getProcAddress "glProgramNamedParameter4fvNV")

{-# NOINLINE glProgramNamedParameter4fvNVFunPtr #-}

pattern GL_FRAGMENT_PROGRAM_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_PROGRAM_BINDING_NV = 0x8873

pattern GL_FRAGMENT_PROGRAM_NV  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_PROGRAM_NV = 0x8870

pattern GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV = 0x8868

pattern GL_MAX_TEXTURE_COORDS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_TEXTURE_COORDS_NV = 0x8871

pattern GL_MAX_TEXTURE_IMAGE_UNITS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_TEXTURE_IMAGE_UNITS_NV = 0x8872

pattern GL_PROGRAM_ERROR_STRING_NV  :: (Eq a, Num a) => a

pattern GL_PROGRAM_ERROR_STRING_NV = 0x8874