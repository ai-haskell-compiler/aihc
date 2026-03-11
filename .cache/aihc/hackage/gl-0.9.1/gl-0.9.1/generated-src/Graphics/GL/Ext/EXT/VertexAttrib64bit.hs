-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.VertexAttrib64bit (
  -- * Extension Support
    gl_EXT_vertex_attrib_64bit

  -- * GL_EXT_vertex_attrib_64bit
  , glGetVertexAttribLdvEXT
  , glVertexAttribL1dEXT
  , glVertexAttribL1dvEXT
  , glVertexAttribL2dEXT
  , glVertexAttribL2dvEXT
  , glVertexAttribL3dEXT
  , glVertexAttribL3dvEXT
  , glVertexAttribL4dEXT
  , glVertexAttribL4dvEXT
  , glVertexAttribLPointerEXT
  , pattern GL_DOUBLE
  , pattern GL_DOUBLE_MAT2_EXT
  , pattern GL_DOUBLE_MAT2x3_EXT
  , pattern GL_DOUBLE_MAT2x4_EXT
  , pattern GL_DOUBLE_MAT3_EXT
  , pattern GL_DOUBLE_MAT3x2_EXT
  , pattern GL_DOUBLE_MAT3x4_EXT
  , pattern GL_DOUBLE_MAT4_EXT
  , pattern GL_DOUBLE_MAT4x2_EXT
  , pattern GL_DOUBLE_MAT4x3_EXT
  , pattern GL_DOUBLE_VEC2_EXT
  , pattern GL_DOUBLE_VEC3_EXT
  , pattern GL_DOUBLE_VEC4_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/vertex_attrib_64bit.txt GL_EXT_vertex_attrib_64bit> extension is available.

gl_EXT_vertex_attrib_64bit :: Bool
gl_EXT_vertex_attrib_64bit = member "GL_EXT_vertex_attrib_64bit" extensions
{-# NOINLINE gl_EXT_vertex_attrib_64bit #-}

-- | Usage: @'glGetVertexAttribLdvEXT' index pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetVertexAttribLdv'.


glGetVertexAttribLdvEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLdouble -> m ()
glGetVertexAttribLdvEXT = ffiuintenumPtrdoubleIOV glGetVertexAttribLdvEXTFunPtr

glGetVertexAttribLdvEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
glGetVertexAttribLdvEXTFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribLdvEXT")

{-# NOINLINE glGetVertexAttribLdvEXTFunPtr #-}

-- | Usage: @'glVertexAttribL1dEXT' index x@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL1d'.


glVertexAttribL1dEXT :: MonadIO m => GLuint -> GLdouble -> m ()
glVertexAttribL1dEXT = ffiuintdoubleIOV glVertexAttribL1dEXTFunPtr

glVertexAttribL1dEXTFunPtr :: FunPtr (GLuint -> GLdouble -> IO ())
glVertexAttribL1dEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1dEXT")

{-# NOINLINE glVertexAttribL1dEXTFunPtr #-}

-- | Usage: @'glVertexAttribL1dvEXT' index v@
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL1dv'.


glVertexAttribL1dvEXT :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttribL1dvEXT = ffiuintPtrdoubleIOV glVertexAttribL1dvEXTFunPtr

glVertexAttribL1dvEXTFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttribL1dvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL1dvEXT")

{-# NOINLINE glVertexAttribL1dvEXTFunPtr #-}

-- | Usage: @'glVertexAttribL2dEXT' index x y@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL2d'.


glVertexAttribL2dEXT :: MonadIO m => GLuint -> GLdouble -> GLdouble -> m ()
glVertexAttribL2dEXT = ffiuintdoubledoubleIOV glVertexAttribL2dEXTFunPtr

glVertexAttribL2dEXTFunPtr :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
glVertexAttribL2dEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL2dEXT")

{-# NOINLINE glVertexAttribL2dEXTFunPtr #-}

-- | Usage: @'glVertexAttribL2dvEXT' index v@
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL2dv'.


glVertexAttribL2dvEXT :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttribL2dvEXT = ffiuintPtrdoubleIOV glVertexAttribL2dvEXTFunPtr

glVertexAttribL2dvEXTFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttribL2dvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL2dvEXT")

{-# NOINLINE glVertexAttribL2dvEXTFunPtr #-}

-- | Usage: @'glVertexAttribL3dEXT' index x y z@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL3d'.


glVertexAttribL3dEXT :: MonadIO m => GLuint -> GLdouble -> GLdouble -> GLdouble -> m ()
glVertexAttribL3dEXT = ffiuintdoubledoubledoubleIOV glVertexAttribL3dEXTFunPtr

glVertexAttribL3dEXTFunPtr :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
glVertexAttribL3dEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL3dEXT")

{-# NOINLINE glVertexAttribL3dEXTFunPtr #-}

-- | Usage: @'glVertexAttribL3dvEXT' index v@
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL3dv'.


glVertexAttribL3dvEXT :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttribL3dvEXT = ffiuintPtrdoubleIOV glVertexAttribL3dvEXTFunPtr

glVertexAttribL3dvEXTFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttribL3dvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL3dvEXT")

{-# NOINLINE glVertexAttribL3dvEXTFunPtr #-}

-- | Usage: @'glVertexAttribL4dEXT' index x y z w@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL4d'.


glVertexAttribL4dEXT :: MonadIO m => GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glVertexAttribL4dEXT = ffiuintdoubledoubledoubledoubleIOV glVertexAttribL4dEXTFunPtr

glVertexAttribL4dEXTFunPtr :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glVertexAttribL4dEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL4dEXT")

{-# NOINLINE glVertexAttribL4dEXTFunPtr #-}

-- | Usage: @'glVertexAttribL4dvEXT' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribL4dv'.


glVertexAttribL4dvEXT :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVertexAttribL4dvEXT = ffiuintPtrdoubleIOV glVertexAttribL4dvEXTFunPtr

glVertexAttribL4dvEXTFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVertexAttribL4dvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribL4dvEXT")

{-# NOINLINE glVertexAttribL4dvEXTFunPtr #-}

-- | Usage: @'glVertexAttribLPointerEXT' index size type stride pointer@
--
-- The length of @pointer@ should be @size@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribLPointer'.


glVertexAttribLPointerEXT :: MonadIO m => GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glVertexAttribLPointerEXT = ffiuintintenumsizeiPtrVIOV glVertexAttribLPointerEXTFunPtr

glVertexAttribLPointerEXTFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glVertexAttribLPointerEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribLPointerEXT")

{-# NOINLINE glVertexAttribLPointerEXTFunPtr #-}

pattern GL_DOUBLE_MAT2_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT2_EXT = 0x8F46

pattern GL_DOUBLE_MAT2x3_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT2x3_EXT = 0x8F49

pattern GL_DOUBLE_MAT2x4_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT2x4_EXT = 0x8F4A

pattern GL_DOUBLE_MAT3_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT3_EXT = 0x8F47

pattern GL_DOUBLE_MAT3x2_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT3x2_EXT = 0x8F4B

pattern GL_DOUBLE_MAT3x4_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT3x4_EXT = 0x8F4C

pattern GL_DOUBLE_MAT4_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT4_EXT = 0x8F48

pattern GL_DOUBLE_MAT4x2_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT4x2_EXT = 0x8F4D

pattern GL_DOUBLE_MAT4x3_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_MAT4x3_EXT = 0x8F4E

pattern GL_DOUBLE_VEC2_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_VEC2_EXT = 0x8FFC

pattern GL_DOUBLE_VEC3_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_VEC3_EXT = 0x8FFD

pattern GL_DOUBLE_VEC4_EXT  :: (Eq a, Num a) => a

pattern GL_DOUBLE_VEC4_EXT = 0x8FFE