-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexProgram4 (
  -- * Extension Support
    gl_NV_vertex_program4

  -- * GL_NV_vertex_program4
  , glGetVertexAttribIivEXT
  , glGetVertexAttribIuivEXT
  , glVertexAttribI1iEXT
  , glVertexAttribI1ivEXT
  , glVertexAttribI1uiEXT
  , glVertexAttribI1uivEXT
  , glVertexAttribI2iEXT
  , glVertexAttribI2ivEXT
  , glVertexAttribI2uiEXT
  , glVertexAttribI2uivEXT
  , glVertexAttribI3iEXT
  , glVertexAttribI3ivEXT
  , glVertexAttribI3uiEXT
  , glVertexAttribI3uivEXT
  , glVertexAttribI4bvEXT
  , glVertexAttribI4iEXT
  , glVertexAttribI4ivEXT
  , glVertexAttribI4svEXT
  , glVertexAttribI4ubvEXT
  , glVertexAttribI4uiEXT
  , glVertexAttribI4uivEXT
  , glVertexAttribI4usvEXT
  , glVertexAttribIPointerEXT
  , pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_program4.txt GL_NV_vertex_program4> extension is available.

gl_NV_vertex_program4 :: Bool
gl_NV_vertex_program4 = member "GL_NV_vertex_program4" extensions
{-# NOINLINE gl_NV_vertex_program4 #-}

-- | Usage: @'glGetVertexAttribIivEXT' index pname params@
--
-- The parameter @pname@ is a @VertexAttribEnum@.
--
-- The length of @params@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetVertexAttribIiv'.


glGetVertexAttribIivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVertexAttribIivEXT = ffiuintenumPtrintIOV glGetVertexAttribIivEXTFunPtr

glGetVertexAttribIivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVertexAttribIivEXTFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribIivEXT")

{-# NOINLINE glGetVertexAttribIivEXTFunPtr #-}

-- | Usage: @'glGetVertexAttribIuivEXT' index pname params@
--
-- The parameter @pname@ is a @VertexAttribEnum@.
--
-- The length of @params@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetVertexAttribIuiv'.


glGetVertexAttribIuivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glGetVertexAttribIuivEXT = ffiuintenumPtruintIOV glGetVertexAttribIuivEXTFunPtr

glGetVertexAttribIuivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glGetVertexAttribIuivEXTFunPtr = unsafePerformIO (getProcAddress "glGetVertexAttribIuivEXT")

{-# NOINLINE glGetVertexAttribIuivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI1iEXT' index x@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI1i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI1ivEXT'.


glVertexAttribI1iEXT :: MonadIO m => GLuint -> GLint -> m ()
glVertexAttribI1iEXT = ffiuintintIOV glVertexAttribI1iEXTFunPtr

glVertexAttribI1iEXTFunPtr :: FunPtr (GLuint -> GLint -> IO ())
glVertexAttribI1iEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI1iEXT")

{-# NOINLINE glVertexAttribI1iEXTFunPtr #-}

-- | Usage: @'glVertexAttribI1ivEXT' index v@
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI1iv'.


glVertexAttribI1ivEXT :: MonadIO m => GLuint -> Ptr GLint -> m ()
glVertexAttribI1ivEXT = ffiuintPtrintIOV glVertexAttribI1ivEXTFunPtr

glVertexAttribI1ivEXTFunPtr :: FunPtr (GLuint -> Ptr GLint -> IO ())
glVertexAttribI1ivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI1ivEXT")

{-# NOINLINE glVertexAttribI1ivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI1uiEXT' index x@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI1ui'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI1uivEXT'.


glVertexAttribI1uiEXT :: MonadIO m => GLuint -> GLuint -> m ()
glVertexAttribI1uiEXT = ffiuintuintIOV glVertexAttribI1uiEXTFunPtr

glVertexAttribI1uiEXTFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glVertexAttribI1uiEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI1uiEXT")

{-# NOINLINE glVertexAttribI1uiEXTFunPtr #-}

-- | Usage: @'glVertexAttribI1uivEXT' index v@
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI1uiv'.


glVertexAttribI1uivEXT :: MonadIO m => GLuint -> Ptr GLuint -> m ()
glVertexAttribI1uivEXT = ffiuintPtruintIOV glVertexAttribI1uivEXTFunPtr

glVertexAttribI1uivEXTFunPtr :: FunPtr (GLuint -> Ptr GLuint -> IO ())
glVertexAttribI1uivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI1uivEXT")

{-# NOINLINE glVertexAttribI1uivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI2iEXT' index x y@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI2i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI2ivEXT'.


glVertexAttribI2iEXT :: MonadIO m => GLuint -> GLint -> GLint -> m ()
glVertexAttribI2iEXT = ffiuintintintIOV glVertexAttribI2iEXTFunPtr

glVertexAttribI2iEXTFunPtr :: FunPtr (GLuint -> GLint -> GLint -> IO ())
glVertexAttribI2iEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI2iEXT")

{-# NOINLINE glVertexAttribI2iEXTFunPtr #-}

-- | Usage: @'glVertexAttribI2ivEXT' index v@
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI2iv'.


glVertexAttribI2ivEXT :: MonadIO m => GLuint -> Ptr GLint -> m ()
glVertexAttribI2ivEXT = ffiuintPtrintIOV glVertexAttribI2ivEXTFunPtr

glVertexAttribI2ivEXTFunPtr :: FunPtr (GLuint -> Ptr GLint -> IO ())
glVertexAttribI2ivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI2ivEXT")

{-# NOINLINE glVertexAttribI2ivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI2uiEXT' index x y@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI2ui'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI2uivEXT'.


glVertexAttribI2uiEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> m ()
glVertexAttribI2uiEXT = ffiuintuintuintIOV glVertexAttribI2uiEXTFunPtr

glVertexAttribI2uiEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
glVertexAttribI2uiEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI2uiEXT")

{-# NOINLINE glVertexAttribI2uiEXTFunPtr #-}

-- | Usage: @'glVertexAttribI2uivEXT' index v@
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI2uiv'.


glVertexAttribI2uivEXT :: MonadIO m => GLuint -> Ptr GLuint -> m ()
glVertexAttribI2uivEXT = ffiuintPtruintIOV glVertexAttribI2uivEXTFunPtr

glVertexAttribI2uivEXTFunPtr :: FunPtr (GLuint -> Ptr GLuint -> IO ())
glVertexAttribI2uivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI2uivEXT")

{-# NOINLINE glVertexAttribI2uivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI3iEXT' index x y z@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI3i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI3ivEXT'.


glVertexAttribI3iEXT :: MonadIO m => GLuint -> GLint -> GLint -> GLint -> m ()
glVertexAttribI3iEXT = ffiuintintintintIOV glVertexAttribI3iEXTFunPtr

glVertexAttribI3iEXTFunPtr :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
glVertexAttribI3iEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI3iEXT")

{-# NOINLINE glVertexAttribI3iEXTFunPtr #-}

-- | Usage: @'glVertexAttribI3ivEXT' index v@
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI3iv'.


glVertexAttribI3ivEXT :: MonadIO m => GLuint -> Ptr GLint -> m ()
glVertexAttribI3ivEXT = ffiuintPtrintIOV glVertexAttribI3ivEXTFunPtr

glVertexAttribI3ivEXTFunPtr :: FunPtr (GLuint -> Ptr GLint -> IO ())
glVertexAttribI3ivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI3ivEXT")

{-# NOINLINE glVertexAttribI3ivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI3uiEXT' index x y z@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI3ui'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI3uivEXT'.


glVertexAttribI3uiEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> GLuint -> m ()
glVertexAttribI3uiEXT = ffiuintuintuintuintIOV glVertexAttribI3uiEXTFunPtr

glVertexAttribI3uiEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glVertexAttribI3uiEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI3uiEXT")

{-# NOINLINE glVertexAttribI3uiEXTFunPtr #-}

-- | Usage: @'glVertexAttribI3uivEXT' index v@
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI3uiv'.


glVertexAttribI3uivEXT :: MonadIO m => GLuint -> Ptr GLuint -> m ()
glVertexAttribI3uivEXT = ffiuintPtruintIOV glVertexAttribI3uivEXTFunPtr

glVertexAttribI3uivEXTFunPtr :: FunPtr (GLuint -> Ptr GLuint -> IO ())
glVertexAttribI3uivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI3uivEXT")

{-# NOINLINE glVertexAttribI3uivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4bvEXT' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4bv'.


glVertexAttribI4bvEXT :: MonadIO m => GLuint -> Ptr GLbyte -> m ()
glVertexAttribI4bvEXT = ffiuintPtrbyteIOV glVertexAttribI4bvEXTFunPtr

glVertexAttribI4bvEXTFunPtr :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
glVertexAttribI4bvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4bvEXT")

{-# NOINLINE glVertexAttribI4bvEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4iEXT' index x y z w@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI4ivEXT'.


glVertexAttribI4iEXT :: MonadIO m => GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
glVertexAttribI4iEXT = ffiuintintintintintIOV glVertexAttribI4iEXTFunPtr

glVertexAttribI4iEXTFunPtr :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
glVertexAttribI4iEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4iEXT")

{-# NOINLINE glVertexAttribI4iEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4ivEXT' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4iv'.


glVertexAttribI4ivEXT :: MonadIO m => GLuint -> Ptr GLint -> m ()
glVertexAttribI4ivEXT = ffiuintPtrintIOV glVertexAttribI4ivEXTFunPtr

glVertexAttribI4ivEXTFunPtr :: FunPtr (GLuint -> Ptr GLint -> IO ())
glVertexAttribI4ivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4ivEXT")

{-# NOINLINE glVertexAttribI4ivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4svEXT' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4sv'.


glVertexAttribI4svEXT :: MonadIO m => GLuint -> Ptr GLshort -> m ()
glVertexAttribI4svEXT = ffiuintPtrshortIOV glVertexAttribI4svEXTFunPtr

glVertexAttribI4svEXTFunPtr :: FunPtr (GLuint -> Ptr GLshort -> IO ())
glVertexAttribI4svEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4svEXT")

{-# NOINLINE glVertexAttribI4svEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4ubvEXT' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4ubv'.


glVertexAttribI4ubvEXT :: MonadIO m => GLuint -> Ptr GLubyte -> m ()
glVertexAttribI4ubvEXT = ffiuintPtrubyteIOV glVertexAttribI4ubvEXTFunPtr

glVertexAttribI4ubvEXTFunPtr :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
glVertexAttribI4ubvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4ubvEXT")

{-# NOINLINE glVertexAttribI4ubvEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4uiEXT' index x y z w@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4ui'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.VertexProgram4.glVertexAttribI4uivEXT'.


glVertexAttribI4uiEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glVertexAttribI4uiEXT = ffiuintuintuintuintuintIOV glVertexAttribI4uiEXTFunPtr

glVertexAttribI4uiEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glVertexAttribI4uiEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4uiEXT")

{-# NOINLINE glVertexAttribI4uiEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4uivEXT' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4uiv'.


glVertexAttribI4uivEXT :: MonadIO m => GLuint -> Ptr GLuint -> m ()
glVertexAttribI4uivEXT = ffiuintPtruintIOV glVertexAttribI4uivEXTFunPtr

glVertexAttribI4uivEXTFunPtr :: FunPtr (GLuint -> Ptr GLuint -> IO ())
glVertexAttribI4uivEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4uivEXT")

{-# NOINLINE glVertexAttribI4uivEXTFunPtr #-}

-- | Usage: @'glVertexAttribI4usvEXT' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribI4usv'.


glVertexAttribI4usvEXT :: MonadIO m => GLuint -> Ptr GLushort -> m ()
glVertexAttribI4usvEXT = ffiuintPtrushortIOV glVertexAttribI4usvEXTFunPtr

glVertexAttribI4usvEXTFunPtr :: FunPtr (GLuint -> Ptr GLushort -> IO ())
glVertexAttribI4usvEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribI4usvEXT")

{-# NOINLINE glVertexAttribI4usvEXTFunPtr #-}

-- | Usage: @'glVertexAttribIPointerEXT' index size type stride pointer@
--
-- The parameter @type@ is a @VertexAttribEnum@.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribIPointer'.


glVertexAttribIPointerEXT :: MonadIO m => GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glVertexAttribIPointerEXT = ffiuintintenumsizeiPtrVIOV glVertexAttribIPointerEXTFunPtr

glVertexAttribIPointerEXTFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glVertexAttribIPointerEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribIPointerEXT")

{-# NOINLINE glVertexAttribIPointerEXTFunPtr #-}

pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER_NV = 0x88FD