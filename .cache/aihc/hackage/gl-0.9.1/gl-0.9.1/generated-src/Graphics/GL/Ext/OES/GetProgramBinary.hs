-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.GetProgramBinary (
  -- * Extension Support
    gl_OES_get_program_binary

  -- * GL_OES_get_program_binary
  , glGetProgramBinaryOES
  , glProgramBinaryOES
  , pattern GL_NUM_PROGRAM_BINARY_FORMATS_OES
  , pattern GL_PROGRAM_BINARY_FORMATS_OES
  , pattern GL_PROGRAM_BINARY_LENGTH_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_get_program_binary.txt GL_OES_get_program_binary> extension is available.

gl_OES_get_program_binary :: Bool
gl_OES_get_program_binary = member "GL_OES_get_program_binary" extensions
{-# NOINLINE gl_OES_get_program_binary #-}

-- | Usage: @'glGetProgramBinaryOES' program bufSize length binaryFormat binary@
--
-- The length of @length@ should be @1@.
--
-- The length of @binaryFormat@ should be @1@.
--
-- The length of @binary@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetProgramBinary'.


glGetProgramBinaryOES :: MonadIO m => GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> m ()
glGetProgramBinaryOES = ffiuintsizeiPtrsizeiPtrenumPtrVIOV glGetProgramBinaryOESFunPtr

glGetProgramBinaryOESFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> IO ())
glGetProgramBinaryOESFunPtr = unsafePerformIO (getProcAddress "glGetProgramBinaryOES")

{-# NOINLINE glGetProgramBinaryOESFunPtr #-}

-- | Usage: @'glProgramBinaryOES' program binaryFormat binary length@
--
-- The length of @binary@ should be @length@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glProgramBinary'.


glProgramBinaryOES :: MonadIO m => GLuint -> GLenum -> Ptr () -> GLint -> m ()
glProgramBinaryOES = ffiuintenumPtrVintIOV glProgramBinaryOESFunPtr

glProgramBinaryOESFunPtr :: FunPtr (GLuint -> GLenum -> Ptr () -> GLint -> IO ())
glProgramBinaryOESFunPtr = unsafePerformIO (getProcAddress "glProgramBinaryOES")

{-# NOINLINE glProgramBinaryOESFunPtr #-}

pattern GL_NUM_PROGRAM_BINARY_FORMATS_OES  :: (Eq a, Num a) => a

pattern GL_NUM_PROGRAM_BINARY_FORMATS_OES = 0x87FE

pattern GL_PROGRAM_BINARY_FORMATS_OES  :: (Eq a, Num a) => a

pattern GL_PROGRAM_BINARY_FORMATS_OES = 0x87FF

pattern GL_PROGRAM_BINARY_LENGTH_OES  :: (Eq a, Num a) => a

pattern GL_PROGRAM_BINARY_LENGTH_OES = 0x8741