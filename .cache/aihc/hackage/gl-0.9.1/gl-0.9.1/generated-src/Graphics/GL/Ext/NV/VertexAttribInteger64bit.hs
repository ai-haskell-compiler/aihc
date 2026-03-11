-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexAttribInteger64bit (
  -- * Extension Support
    gl_NV_vertex_attrib_integer_64bit

  -- * GL_NV_vertex_attrib_integer_64bit
  , glGetVertexAttribLi64vNV
  , glGetVertexAttribLui64vNV
  , glVertexAttribL1i64NV
  , glVertexAttribL1i64vNV
  , glVertexAttribL1ui64NV
  , glVertexAttribL1ui64vNV
  , glVertexAttribL2i64NV
  , glVertexAttribL2i64vNV
  , glVertexAttribL2ui64NV
  , glVertexAttribL2ui64vNV
  , glVertexAttribL3i64NV
  , glVertexAttribL3i64vNV
  , glVertexAttribL3ui64NV
  , glVertexAttribL3ui64vNV
  , glVertexAttribL4i64NV
  , glVertexAttribL4i64vNV
  , glVertexAttribL4ui64NV
  , glVertexAttribL4ui64vNV
  , glVertexAttribLFormatNV
  , pattern GL_INT64_NV
  , pattern GL_UNSIGNED_INT64_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_attrib_integer_64bit.txt GL_NV_vertex_attrib_integer_64bit> extension is available.

gl_NV_vertex_attrib_integer_64bit :: Bool
gl_NV_vertex_attrib_integer_64bit = member "GL_NV_vertex_attrib_integer_64bit" extensions
{-# NOINLINE gl_NV_vertex_attrib_integer_64bit #-}

-- | Usage: @'glGetVertexAttribLi64vNV' index pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVertexAttribLi64vNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint64EXT -> m ()
glGetVertexAttribLi64vNV = ffiuintenumPtrint64EXTIOV glGetVertexAttribLi64vNVFunPtr

glGetVertexAttribLi64vNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ())
glGetVertexAttribLi64vNVFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribLi64vNV")

{-# NOINLINE glGetVertexAttribLi64vNVFunPtr #-}

-- | Usage: @'glGetVertexAttribLui64vNV' index pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetVertexAttribLui64vNV :: MonadIO m => GLuint -> GLenum -> Ptr GLuint64EXT -> m ()
glGetVertexAttribLui64vNV = ffiuintenumPtruint64EXTIOV glGetVertexAttribLui64vNVFunPtr

glGetVertexAttribLui64vNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
glGetVertexAttribLui64vNVFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribLui64vNV")

{-# NOINLINE glGetVertexAttribLui64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL1i64NV' index x@


glVertexAttribL1i64NV :: MonadIO m => GLuint -> GLint64EXT -> m ()
glVertexAttribL1i64NV = ffiuintint64EXTIOV glVertexAttribL1i64NVFunPtr

glVertexAttribL1i64NVFunPtr :: FunPtr (GLuint -> GLint64EXT -> IO ())
glVertexAttribL1i64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1i64NV")

{-# NOINLINE glVertexAttribL1i64NVFunPtr #-}

-- | Usage: @'glVertexAttribL1i64vNV' index v@
--
-- The length of @v@ should be @1@.


glVertexAttribL1i64vNV :: MonadIO m => GLuint -> Ptr GLint64EXT -> m ()
glVertexAttribL1i64vNV = ffiuintPtrint64EXTIOV glVertexAttribL1i64vNVFunPtr

glVertexAttribL1i64vNVFunPtr :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
glVertexAttribL1i64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1i64vNV")

{-# NOINLINE glVertexAttribL1i64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL1ui64NV' index x@


glVertexAttribL1ui64NV :: MonadIO m => GLuint -> GLuint64EXT -> m ()
glVertexAttribL1ui64NV = ffiuintuint64EXTIOV glVertexAttribL1ui64NVFunPtr

glVertexAttribL1ui64NVFunPtr :: FunPtr (GLuint -> GLuint64EXT -> IO ())
glVertexAttribL1ui64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1ui64NV")

{-# NOINLINE glVertexAttribL1ui64NVFunPtr #-}

-- | Usage: @'glVertexAttribL1ui64vNV' index v@
--
-- The length of @v@ should be @1@.


glVertexAttribL1ui64vNV :: MonadIO m => GLuint -> Ptr GLuint64EXT -> m ()
glVertexAttribL1ui64vNV = ffiuintPtruint64EXTIOV glVertexAttribL1ui64vNVFunPtr

glVertexAttribL1ui64vNVFunPtr :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
glVertexAttribL1ui64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1ui64vNV")

{-# NOINLINE glVertexAttribL1ui64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL2i64NV' index x y@


glVertexAttribL2i64NV :: MonadIO m => GLuint -> GLint64EXT -> GLint64EXT -> m ()
glVertexAttribL2i64NV = ffiuintint64EXTint64EXTIOV glVertexAttribL2i64NVFunPtr

glVertexAttribL2i64NVFunPtr :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> IO ())
glVertexAttribL2i64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL2i64NV")

{-# NOINLINE glVertexAttribL2i64NVFunPtr #-}

-- | Usage: @'glVertexAttribL2i64vNV' index v@
--
-- The length of @v@ should be @2@.


glVertexAttribL2i64vNV :: MonadIO m => GLuint -> Ptr GLint64EXT -> m ()
glVertexAttribL2i64vNV = ffiuintPtrint64EXTIOV glVertexAttribL2i64vNVFunPtr

glVertexAttribL2i64vNVFunPtr :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
glVertexAttribL2i64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL2i64vNV")

{-# NOINLINE glVertexAttribL2i64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL2ui64NV' index x y@


glVertexAttribL2ui64NV :: MonadIO m => GLuint -> GLuint64EXT -> GLuint64EXT -> m ()
glVertexAttribL2ui64NV = ffiuintuint64EXTuint64EXTIOV glVertexAttribL2ui64NVFunPtr

glVertexAttribL2ui64NVFunPtr :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> IO ())
glVertexAttribL2ui64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL2ui64NV")

{-# NOINLINE glVertexAttribL2ui64NVFunPtr #-}

-- | Usage: @'glVertexAttribL2ui64vNV' index v@
--
-- The length of @v@ should be @2@.


glVertexAttribL2ui64vNV :: MonadIO m => GLuint -> Ptr GLuint64EXT -> m ()
glVertexAttribL2ui64vNV = ffiuintPtruint64EXTIOV glVertexAttribL2ui64vNVFunPtr

glVertexAttribL2ui64vNVFunPtr :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
glVertexAttribL2ui64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL2ui64vNV")

{-# NOINLINE glVertexAttribL2ui64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL3i64NV' index x y z@


glVertexAttribL3i64NV :: MonadIO m => GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
glVertexAttribL3i64NV = ffiuintint64EXTint64EXTint64EXTIOV glVertexAttribL3i64NVFunPtr

glVertexAttribL3i64NVFunPtr :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
glVertexAttribL3i64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL3i64NV")

{-# NOINLINE glVertexAttribL3i64NVFunPtr #-}

-- | Usage: @'glVertexAttribL3i64vNV' index v@
--
-- The length of @v@ should be @3@.


glVertexAttribL3i64vNV :: MonadIO m => GLuint -> Ptr GLint64EXT -> m ()
glVertexAttribL3i64vNV = ffiuintPtrint64EXTIOV glVertexAttribL3i64vNVFunPtr

glVertexAttribL3i64vNVFunPtr :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
glVertexAttribL3i64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL3i64vNV")

{-# NOINLINE glVertexAttribL3i64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL3ui64NV' index x y z@


glVertexAttribL3ui64NV :: MonadIO m => GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
glVertexAttribL3ui64NV = ffiuintuint64EXTuint64EXTuint64EXTIOV glVertexAttribL3ui64NVFunPtr

glVertexAttribL3ui64NVFunPtr :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
glVertexAttribL3ui64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL3ui64NV")

{-# NOINLINE glVertexAttribL3ui64NVFunPtr #-}

-- | Usage: @'glVertexAttribL3ui64vNV' index v@
--
-- The length of @v@ should be @3@.


glVertexAttribL3ui64vNV :: MonadIO m => GLuint -> Ptr GLuint64EXT -> m ()
glVertexAttribL3ui64vNV = ffiuintPtruint64EXTIOV glVertexAttribL3ui64vNVFunPtr

glVertexAttribL3ui64vNVFunPtr :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
glVertexAttribL3ui64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL3ui64vNV")

{-# NOINLINE glVertexAttribL3ui64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL4i64NV' index x y z w@


glVertexAttribL4i64NV :: MonadIO m => GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
glVertexAttribL4i64NV = ffiuintint64EXTint64EXTint64EXTint64EXTIOV glVertexAttribL4i64NVFunPtr

glVertexAttribL4i64NVFunPtr :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
glVertexAttribL4i64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL4i64NV")

{-# NOINLINE glVertexAttribL4i64NVFunPtr #-}

-- | Usage: @'glVertexAttribL4i64vNV' index v@
--
-- The length of @v@ should be @4@.


glVertexAttribL4i64vNV :: MonadIO m => GLuint -> Ptr GLint64EXT -> m ()
glVertexAttribL4i64vNV = ffiuintPtrint64EXTIOV glVertexAttribL4i64vNVFunPtr

glVertexAttribL4i64vNVFunPtr :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
glVertexAttribL4i64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL4i64vNV")

{-# NOINLINE glVertexAttribL4i64vNVFunPtr #-}

-- | Usage: @'glVertexAttribL4ui64NV' index x y z w@


glVertexAttribL4ui64NV :: MonadIO m => GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
glVertexAttribL4ui64NV = ffiuintuint64EXTuint64EXTuint64EXTuint64EXTIOV glVertexAttribL4ui64NVFunPtr

glVertexAttribL4ui64NVFunPtr :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
glVertexAttribL4ui64NVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL4ui64NV")

{-# NOINLINE glVertexAttribL4ui64NVFunPtr #-}

-- | Usage: @'glVertexAttribL4ui64vNV' index v@
--
-- The length of @v@ should be @4@.


glVertexAttribL4ui64vNV :: MonadIO m => GLuint -> Ptr GLuint64EXT -> m ()
glVertexAttribL4ui64vNV = ffiuintPtruint64EXTIOV glVertexAttribL4ui64vNVFunPtr

glVertexAttribL4ui64vNVFunPtr :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
glVertexAttribL4ui64vNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL4ui64vNV")

{-# NOINLINE glVertexAttribL4ui64vNVFunPtr #-}

-- | Usage: @'glVertexAttribLFormatNV' index size type stride@


glVertexAttribLFormatNV :: MonadIO m => GLuint -> GLint -> GLenum -> GLsizei -> m ()
glVertexAttribLFormatNV = ffiuintintenumsizeiIOV glVertexAttribLFormatNVFunPtr

glVertexAttribLFormatNVFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ())
glVertexAttribLFormatNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribLFormatNV")

{-# NOINLINE glVertexAttribLFormatNVFunPtr #-}