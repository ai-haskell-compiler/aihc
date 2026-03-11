-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.GpuShaderInt64 (
  -- * Extension Support
    gl_ARB_gpu_shader_int64

  -- * GL_ARB_gpu_shader_int64
  , glGetUniformi64vARB
  , glGetUniformui64vARB
  , glGetnUniformi64vARB
  , glGetnUniformui64vARB
  , glProgramUniform1i64ARB
  , glProgramUniform1i64vARB
  , glProgramUniform1ui64ARB
  , glProgramUniform1ui64vARB
  , glProgramUniform2i64ARB
  , glProgramUniform2i64vARB
  , glProgramUniform2ui64ARB
  , glProgramUniform2ui64vARB
  , glProgramUniform3i64ARB
  , glProgramUniform3i64vARB
  , glProgramUniform3ui64ARB
  , glProgramUniform3ui64vARB
  , glProgramUniform4i64ARB
  , glProgramUniform4i64vARB
  , glProgramUniform4ui64ARB
  , glProgramUniform4ui64vARB
  , glUniform1i64ARB
  , glUniform1i64vARB
  , glUniform1ui64ARB
  , glUniform1ui64vARB
  , glUniform2i64ARB
  , glUniform2i64vARB
  , glUniform2ui64ARB
  , glUniform2ui64vARB
  , glUniform3i64ARB
  , glUniform3i64vARB
  , glUniform3ui64ARB
  , glUniform3ui64vARB
  , glUniform4i64ARB
  , glUniform4i64vARB
  , glUniform4ui64ARB
  , glUniform4ui64vARB
  , pattern GL_INT64_ARB
  , pattern GL_INT64_VEC2_ARB
  , pattern GL_INT64_VEC3_ARB
  , pattern GL_INT64_VEC4_ARB
  , pattern GL_UNSIGNED_INT64_ARB
  , pattern GL_UNSIGNED_INT64_VEC2_ARB
  , pattern GL_UNSIGNED_INT64_VEC3_ARB
  , pattern GL_UNSIGNED_INT64_VEC4_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/gpu_shader_int64.txt GL_ARB_gpu_shader_int64> extension is available.

gl_ARB_gpu_shader_int64 :: Bool
gl_ARB_gpu_shader_int64 = member "GL_ARB_gpu_shader_int64" extensions
{-# NOINLINE gl_ARB_gpu_shader_int64 #-}

-- | Usage: @'glGetUniformi64vARB' program location params@
--
-- The length of @params@ should be @COMPSIZE(program,location)@.


glGetUniformi64vARB :: MonadIO m => GLuint -> GLint -> Ptr GLint64 -> m ()
glGetUniformi64vARB = ffiuintintPtrint64IOV glGetUniformi64vARBFunPtr

glGetUniformi64vARBFunPtr :: FunPtr (GLuint -> GLint -> Ptr GLint64 -> IO ())
glGetUniformi64vARBFunPtr = unsafePerformIO (getProcAddress "glGetUniformi64vARB")

{-# NOINLINE glGetUniformi64vARBFunPtr #-}

-- | Usage: @'glGetUniformui64vARB' program location params@
--
-- The length of @params@ should be @COMPSIZE(program,location)@.


glGetUniformui64vARB :: MonadIO m => GLuint -> GLint -> Ptr GLuint64 -> m ()
glGetUniformui64vARB = ffiuintintPtruint64IOV glGetUniformui64vARBFunPtr

glGetUniformui64vARBFunPtr :: FunPtr (GLuint -> GLint -> Ptr GLuint64 -> IO ())
glGetUniformui64vARBFunPtr = unsafePerformIO (getProcAddress "glGetUniformui64vARB")

{-# NOINLINE glGetUniformui64vARBFunPtr #-}

-- | Usage: @'glGetnUniformi64vARB' program location bufSize params@


glGetnUniformi64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint64 -> m ()
glGetnUniformi64vARB = ffiuintintsizeiPtrint64IOV glGetnUniformi64vARBFunPtr

glGetnUniformi64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
glGetnUniformi64vARBFunPtr = unsafePerformIO (getProcAddress "glGetnUniformi64vARB")

{-# NOINLINE glGetnUniformi64vARBFunPtr #-}

-- | Usage: @'glGetnUniformui64vARB' program location bufSize params@


glGetnUniformui64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glGetnUniformui64vARB = ffiuintintsizeiPtruint64IOV glGetnUniformui64vARBFunPtr

glGetnUniformui64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glGetnUniformui64vARBFunPtr = unsafePerformIO (getProcAddress "glGetnUniformui64vARB")

{-# NOINLINE glGetnUniformui64vARBFunPtr #-}

-- | Usage: @'glProgramUniform1i64ARB' program location x@


glProgramUniform1i64ARB :: MonadIO m => GLuint -> GLint -> GLint64 -> m ()
glProgramUniform1i64ARB = ffiuintintint64IOV glProgramUniform1i64ARBFunPtr

glProgramUniform1i64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLint64 -> IO ())
glProgramUniform1i64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform1i64ARB")

{-# NOINLINE glProgramUniform1i64ARBFunPtr #-}

-- | Usage: @'glProgramUniform1i64vARB' program location count value@
--
-- The length of @value@ should be @count@.


glProgramUniform1i64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint64 -> m ()
glProgramUniform1i64vARB = ffiuintintsizeiPtrint64IOV glProgramUniform1i64vARBFunPtr

glProgramUniform1i64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
glProgramUniform1i64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform1i64vARB")

{-# NOINLINE glProgramUniform1i64vARBFunPtr #-}

-- | Usage: @'glProgramUniform1ui64ARB' program location x@


glProgramUniform1ui64ARB :: MonadIO m => GLuint -> GLint -> GLuint64 -> m ()
glProgramUniform1ui64ARB = ffiuintintuint64IOV glProgramUniform1ui64ARBFunPtr

glProgramUniform1ui64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
glProgramUniform1ui64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform1ui64ARB")

{-# NOINLINE glProgramUniform1ui64ARBFunPtr #-}

-- | Usage: @'glProgramUniform1ui64vARB' program location count value@
--
-- The length of @value@ should be @count@.


glProgramUniform1ui64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glProgramUniform1ui64vARB = ffiuintintsizeiPtruint64IOV glProgramUniform1ui64vARBFunPtr

glProgramUniform1ui64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glProgramUniform1ui64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform1ui64vARB")

{-# NOINLINE glProgramUniform1ui64vARBFunPtr #-}

-- | Usage: @'glProgramUniform2i64ARB' program location x y@


glProgramUniform2i64ARB :: MonadIO m => GLuint -> GLint -> GLint64 -> GLint64 -> m ()
glProgramUniform2i64ARB = ffiuintintint64int64IOV glProgramUniform2i64ARBFunPtr

glProgramUniform2i64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> IO ())
glProgramUniform2i64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform2i64ARB")

{-# NOINLINE glProgramUniform2i64ARBFunPtr #-}

-- | Usage: @'glProgramUniform2i64vARB' program location count value@
--
-- The length of @value@ should be @count*2@.


glProgramUniform2i64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint64 -> m ()
glProgramUniform2i64vARB = ffiuintintsizeiPtrint64IOV glProgramUniform2i64vARBFunPtr

glProgramUniform2i64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
glProgramUniform2i64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform2i64vARB")

{-# NOINLINE glProgramUniform2i64vARBFunPtr #-}

-- | Usage: @'glProgramUniform2ui64ARB' program location x y@


glProgramUniform2ui64ARB :: MonadIO m => GLuint -> GLint -> GLuint64 -> GLuint64 -> m ()
glProgramUniform2ui64ARB = ffiuintintuint64uint64IOV glProgramUniform2ui64ARBFunPtr

glProgramUniform2ui64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> IO ())
glProgramUniform2ui64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform2ui64ARB")

{-# NOINLINE glProgramUniform2ui64ARBFunPtr #-}

-- | Usage: @'glProgramUniform2ui64vARB' program location count value@
--
-- The length of @value@ should be @count*2@.


glProgramUniform2ui64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glProgramUniform2ui64vARB = ffiuintintsizeiPtruint64IOV glProgramUniform2ui64vARBFunPtr

glProgramUniform2ui64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glProgramUniform2ui64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform2ui64vARB")

{-# NOINLINE glProgramUniform2ui64vARBFunPtr #-}

-- | Usage: @'glProgramUniform3i64ARB' program location x y z@


glProgramUniform3i64ARB :: MonadIO m => GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> m ()
glProgramUniform3i64ARB = ffiuintintint64int64int64IOV glProgramUniform3i64ARBFunPtr

glProgramUniform3i64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> IO ())
glProgramUniform3i64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform3i64ARB")

{-# NOINLINE glProgramUniform3i64ARBFunPtr #-}

-- | Usage: @'glProgramUniform3i64vARB' program location count value@
--
-- The length of @value@ should be @count*3@.


glProgramUniform3i64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint64 -> m ()
glProgramUniform3i64vARB = ffiuintintsizeiPtrint64IOV glProgramUniform3i64vARBFunPtr

glProgramUniform3i64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
glProgramUniform3i64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform3i64vARB")

{-# NOINLINE glProgramUniform3i64vARBFunPtr #-}

-- | Usage: @'glProgramUniform3ui64ARB' program location x y z@


glProgramUniform3ui64ARB :: MonadIO m => GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
glProgramUniform3ui64ARB = ffiuintintuint64uint64uint64IOV glProgramUniform3ui64ARBFunPtr

glProgramUniform3ui64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
glProgramUniform3ui64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform3ui64ARB")

{-# NOINLINE glProgramUniform3ui64ARBFunPtr #-}

-- | Usage: @'glProgramUniform3ui64vARB' program location count value@
--
-- The length of @value@ should be @count*3@.


glProgramUniform3ui64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glProgramUniform3ui64vARB = ffiuintintsizeiPtruint64IOV glProgramUniform3ui64vARBFunPtr

glProgramUniform3ui64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glProgramUniform3ui64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform3ui64vARB")

{-# NOINLINE glProgramUniform3ui64vARBFunPtr #-}

-- | Usage: @'glProgramUniform4i64ARB' program location x y z w@


glProgramUniform4i64ARB :: MonadIO m => GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> m ()
glProgramUniform4i64ARB = ffiuintintint64int64int64int64IOV glProgramUniform4i64ARBFunPtr

glProgramUniform4i64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ())
glProgramUniform4i64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform4i64ARB")

{-# NOINLINE glProgramUniform4i64ARBFunPtr #-}

-- | Usage: @'glProgramUniform4i64vARB' program location count value@
--
-- The length of @value@ should be @count*4@.


glProgramUniform4i64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint64 -> m ()
glProgramUniform4i64vARB = ffiuintintsizeiPtrint64IOV glProgramUniform4i64vARBFunPtr

glProgramUniform4i64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
glProgramUniform4i64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform4i64vARB")

{-# NOINLINE glProgramUniform4i64vARBFunPtr #-}

-- | Usage: @'glProgramUniform4ui64ARB' program location x y z w@


glProgramUniform4ui64ARB :: MonadIO m => GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
glProgramUniform4ui64ARB = ffiuintintuint64uint64uint64uint64IOV glProgramUniform4ui64ARBFunPtr

glProgramUniform4ui64ARBFunPtr :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
glProgramUniform4ui64ARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform4ui64ARB")

{-# NOINLINE glProgramUniform4ui64ARBFunPtr #-}

-- | Usage: @'glProgramUniform4ui64vARB' program location count value@
--
-- The length of @value@ should be @count*4@.


glProgramUniform4ui64vARB :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
glProgramUniform4ui64vARB = ffiuintintsizeiPtruint64IOV glProgramUniform4ui64vARBFunPtr

glProgramUniform4ui64vARBFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glProgramUniform4ui64vARBFunPtr = unsafePerformIO (getProcAddress "glProgramUniform4ui64vARB")

{-# NOINLINE glProgramUniform4ui64vARBFunPtr #-}

-- | Usage: @'glUniform1i64ARB' location x@


glUniform1i64ARB :: MonadIO m => GLint -> GLint64 -> m ()
glUniform1i64ARB = ffiintint64IOV glUniform1i64ARBFunPtr

glUniform1i64ARBFunPtr :: FunPtr (GLint -> GLint64 -> IO ())
glUniform1i64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform1i64ARB")

{-# NOINLINE glUniform1i64ARBFunPtr #-}

-- | Usage: @'glUniform1i64vARB' location count value@
--
-- The length of @value@ should be @count*1@.


glUniform1i64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint64 -> m ()
glUniform1i64vARB = ffiintsizeiPtrint64IOV glUniform1i64vARBFunPtr

glUniform1i64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
glUniform1i64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform1i64vARB")

{-# NOINLINE glUniform1i64vARBFunPtr #-}

-- | Usage: @'glUniform1ui64ARB' location x@


glUniform1ui64ARB :: MonadIO m => GLint -> GLuint64 -> m ()
glUniform1ui64ARB = ffiintuint64IOV glUniform1ui64ARBFunPtr

glUniform1ui64ARBFunPtr :: FunPtr (GLint -> GLuint64 -> IO ())
glUniform1ui64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform1ui64ARB")

{-# NOINLINE glUniform1ui64ARBFunPtr #-}

-- | Usage: @'glUniform1ui64vARB' location count value@
--
-- The length of @value@ should be @count*1@.


glUniform1ui64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64 -> m ()
glUniform1ui64vARB = ffiintsizeiPtruint64IOV glUniform1ui64vARBFunPtr

glUniform1ui64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glUniform1ui64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform1ui64vARB")

{-# NOINLINE glUniform1ui64vARBFunPtr #-}

-- | Usage: @'glUniform2i64ARB' location x y@


glUniform2i64ARB :: MonadIO m => GLint -> GLint64 -> GLint64 -> m ()
glUniform2i64ARB = ffiintint64int64IOV glUniform2i64ARBFunPtr

glUniform2i64ARBFunPtr :: FunPtr (GLint -> GLint64 -> GLint64 -> IO ())
glUniform2i64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform2i64ARB")

{-# NOINLINE glUniform2i64ARBFunPtr #-}

-- | Usage: @'glUniform2i64vARB' location count value@
--
-- The length of @value@ should be @count*2@.


glUniform2i64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint64 -> m ()
glUniform2i64vARB = ffiintsizeiPtrint64IOV glUniform2i64vARBFunPtr

glUniform2i64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
glUniform2i64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform2i64vARB")

{-# NOINLINE glUniform2i64vARBFunPtr #-}

-- | Usage: @'glUniform2ui64ARB' location x y@


glUniform2ui64ARB :: MonadIO m => GLint -> GLuint64 -> GLuint64 -> m ()
glUniform2ui64ARB = ffiintuint64uint64IOV glUniform2ui64ARBFunPtr

glUniform2ui64ARBFunPtr :: FunPtr (GLint -> GLuint64 -> GLuint64 -> IO ())
glUniform2ui64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform2ui64ARB")

{-# NOINLINE glUniform2ui64ARBFunPtr #-}

-- | Usage: @'glUniform2ui64vARB' location count value@
--
-- The length of @value@ should be @count*2@.


glUniform2ui64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64 -> m ()
glUniform2ui64vARB = ffiintsizeiPtruint64IOV glUniform2ui64vARBFunPtr

glUniform2ui64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glUniform2ui64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform2ui64vARB")

{-# NOINLINE glUniform2ui64vARBFunPtr #-}

-- | Usage: @'glUniform3i64ARB' location x y z@


glUniform3i64ARB :: MonadIO m => GLint -> GLint64 -> GLint64 -> GLint64 -> m ()
glUniform3i64ARB = ffiintint64int64int64IOV glUniform3i64ARBFunPtr

glUniform3i64ARBFunPtr :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> IO ())
glUniform3i64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform3i64ARB")

{-# NOINLINE glUniform3i64ARBFunPtr #-}

-- | Usage: @'glUniform3i64vARB' location count value@
--
-- The length of @value@ should be @count*3@.


glUniform3i64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint64 -> m ()
glUniform3i64vARB = ffiintsizeiPtrint64IOV glUniform3i64vARBFunPtr

glUniform3i64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
glUniform3i64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform3i64vARB")

{-# NOINLINE glUniform3i64vARBFunPtr #-}

-- | Usage: @'glUniform3ui64ARB' location x y z@


glUniform3ui64ARB :: MonadIO m => GLint -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
glUniform3ui64ARB = ffiintuint64uint64uint64IOV glUniform3ui64ARBFunPtr

glUniform3ui64ARBFunPtr :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
glUniform3ui64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform3ui64ARB")

{-# NOINLINE glUniform3ui64ARBFunPtr #-}

-- | Usage: @'glUniform3ui64vARB' location count value@
--
-- The length of @value@ should be @count*3@.


glUniform3ui64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64 -> m ()
glUniform3ui64vARB = ffiintsizeiPtruint64IOV glUniform3ui64vARBFunPtr

glUniform3ui64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glUniform3ui64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform3ui64vARB")

{-# NOINLINE glUniform3ui64vARBFunPtr #-}

-- | Usage: @'glUniform4i64ARB' location x y z w@


glUniform4i64ARB :: MonadIO m => GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> m ()
glUniform4i64ARB = ffiintint64int64int64int64IOV glUniform4i64ARBFunPtr

glUniform4i64ARBFunPtr :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ())
glUniform4i64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform4i64ARB")

{-# NOINLINE glUniform4i64ARBFunPtr #-}

-- | Usage: @'glUniform4i64vARB' location count value@
--
-- The length of @value@ should be @count*4@.


glUniform4i64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLint64 -> m ()
glUniform4i64vARB = ffiintsizeiPtrint64IOV glUniform4i64vARBFunPtr

glUniform4i64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
glUniform4i64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform4i64vARB")

{-# NOINLINE glUniform4i64vARBFunPtr #-}

-- | Usage: @'glUniform4ui64ARB' location x y z w@


glUniform4ui64ARB :: MonadIO m => GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
glUniform4ui64ARB = ffiintuint64uint64uint64uint64IOV glUniform4ui64ARBFunPtr

glUniform4ui64ARBFunPtr :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
glUniform4ui64ARBFunPtr = unsafePerformIO (getProcAddress "glUniform4ui64ARB")

{-# NOINLINE glUniform4ui64ARBFunPtr #-}

-- | Usage: @'glUniform4ui64vARB' location count value@
--
-- The length of @value@ should be @count*4@.


glUniform4ui64vARB :: MonadIO m => GLint -> GLsizei -> Ptr GLuint64 -> m ()
glUniform4ui64vARB = ffiintsizeiPtruint64IOV glUniform4ui64vARBFunPtr

glUniform4ui64vARBFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
glUniform4ui64vARBFunPtr = unsafePerformIO (getProcAddress "glUniform4ui64vARB")

{-# NOINLINE glUniform4ui64vARBFunPtr #-}

pattern GL_INT64_ARB  :: (Eq a, Num a) => a

pattern GL_INT64_ARB = 0x140E

pattern GL_INT64_VEC2_ARB  :: (Eq a, Num a) => a

pattern GL_INT64_VEC2_ARB = 0x8FE9

pattern GL_INT64_VEC3_ARB  :: (Eq a, Num a) => a

pattern GL_INT64_VEC3_ARB = 0x8FEA

pattern GL_INT64_VEC4_ARB  :: (Eq a, Num a) => a

pattern GL_INT64_VEC4_ARB = 0x8FEB

pattern GL_UNSIGNED_INT64_VEC2_ARB  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT64_VEC2_ARB = 0x8FF5

pattern GL_UNSIGNED_INT64_VEC3_ARB  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT64_VEC3_ARB = 0x8FF6

pattern GL_UNSIGNED_INT64_VEC4_ARB  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT64_VEC4_ARB = 0x8FF7