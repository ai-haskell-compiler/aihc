-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BaseInstance (
  -- * Extension Support
    gl_EXT_base_instance

  -- * GL_EXT_base_instance
  , glDrawArraysInstancedBaseInstanceEXT
  , glDrawElementsInstancedBaseInstanceEXT
  , glDrawElementsInstancedBaseVertexBaseInstanceEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_EXT_base_instance extension is available.

gl_EXT_base_instance :: Bool
gl_EXT_base_instance = member "GL_EXT_base_instance" extensions
{-# NOINLINE gl_EXT_base_instance #-}

-- | Usage: @'glDrawArraysInstancedBaseInstanceEXT' mode first count instancecount baseinstance@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawArraysInstancedBaseInstance'.


glDrawArraysInstancedBaseInstanceEXT :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> m ()
glDrawArraysInstancedBaseInstanceEXT = ffienumintsizeisizeiuintIOV glDrawArraysInstancedBaseInstanceEXTFunPtr

glDrawArraysInstancedBaseInstanceEXTFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ())
glDrawArraysInstancedBaseInstanceEXTFunPtr = unsafePerformIO (getProcAddress "glDrawArraysInstancedBaseInstanceEXT")

{-# NOINLINE glDrawArraysInstancedBaseInstanceEXTFunPtr #-}

-- | Usage: @'glDrawElementsInstancedBaseInstanceEXT' mode count type indices instancecount baseinstance@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The length of @indices@ should be @count@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsInstancedBaseInstance'.


glDrawElementsInstancedBaseInstanceEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLuint -> m ()
glDrawElementsInstancedBaseInstanceEXT = ffienumsizeienumPtrVsizeiuintIOV glDrawElementsInstancedBaseInstanceEXTFunPtr

glDrawElementsInstancedBaseInstanceEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLuint -> IO ())
glDrawElementsInstancedBaseInstanceEXTFunPtr = unsafePerformIO (getProcAddress "glDrawElementsInstancedBaseInstanceEXT")

{-# NOINLINE glDrawElementsInstancedBaseInstanceEXTFunPtr #-}

-- | Usage: @'glDrawElementsInstancedBaseVertexBaseInstanceEXT' mode count type indices instancecount basevertex baseinstance@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The length of @indices@ should be @count@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsInstancedBaseVertexBaseInstance'.


glDrawElementsInstancedBaseVertexBaseInstanceEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GLuint -> m ()
glDrawElementsInstancedBaseVertexBaseInstanceEXT = ffienumsizeienumPtrVsizeiintuintIOV glDrawElementsInstancedBaseVertexBaseInstanceEXTFunPtr

glDrawElementsInstancedBaseVertexBaseInstanceEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GLuint -> IO ())
glDrawElementsInstancedBaseVertexBaseInstanceEXTFunPtr = unsafePerformIO (getProcAddress "glDrawElementsInstancedBaseVertexBaseInstanceEXT")

{-# NOINLINE glDrawElementsInstancedBaseVertexBaseInstanceEXTFunPtr #-}