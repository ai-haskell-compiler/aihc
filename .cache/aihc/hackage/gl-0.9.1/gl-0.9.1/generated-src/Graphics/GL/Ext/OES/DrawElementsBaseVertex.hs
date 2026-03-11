-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.DrawElementsBaseVertex (
  -- * Extension Support
    gl_OES_draw_elements_base_vertex

  -- * GL_OES_draw_elements_base_vertex
  , glDrawElementsBaseVertexOES
  , glDrawElementsInstancedBaseVertexOES
  , glDrawRangeElementsBaseVertexOES
  , glMultiDrawElementsBaseVertexOES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_draw_elements_base_vertex.txt GL_OES_draw_elements_base_vertex> extension is available.

gl_OES_draw_elements_base_vertex :: Bool
gl_OES_draw_elements_base_vertex = member "GL_OES_draw_elements_base_vertex" extensions
{-# NOINLINE gl_OES_draw_elements_base_vertex #-}

-- | Usage: @'glDrawElementsBaseVertexOES' mode count type indices basevertex@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsBaseVertex'.


glDrawElementsBaseVertexOES :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> m ()
glDrawElementsBaseVertexOES = ffienumsizeienumPtrVintIOV glDrawElementsBaseVertexOESFunPtr

glDrawElementsBaseVertexOESFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ())
glDrawElementsBaseVertexOESFunPtr = unsafePerformIO (getProcAddress "glDrawElementsBaseVertexOES")

{-# NOINLINE glDrawElementsBaseVertexOESFunPtr #-}

-- | Usage: @'glDrawElementsInstancedBaseVertexOES' mode count type indices instancecount basevertex@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsInstancedBaseVertex'.


glDrawElementsInstancedBaseVertexOES :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> m ()
glDrawElementsInstancedBaseVertexOES = ffienumsizeienumPtrVsizeiintIOV glDrawElementsInstancedBaseVertexOESFunPtr

glDrawElementsInstancedBaseVertexOESFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> IO ())
glDrawElementsInstancedBaseVertexOESFunPtr = unsafePerformIO (getProcAddress "glDrawElementsInstancedBaseVertexOES")

{-# NOINLINE glDrawElementsInstancedBaseVertexOESFunPtr #-}

-- | Usage: @'glDrawRangeElementsBaseVertexOES' mode start end count type indices basevertex@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawRangeElementsBaseVertex'.


glDrawRangeElementsBaseVertexOES :: MonadIO m => GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> m ()
glDrawRangeElementsBaseVertexOES = ffienumuintuintsizeienumPtrVintIOV glDrawRangeElementsBaseVertexOESFunPtr

glDrawRangeElementsBaseVertexOESFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ())
glDrawRangeElementsBaseVertexOESFunPtr = unsafePerformIO (getProcAddress "glDrawRangeElementsBaseVertexOES")

{-# NOINLINE glDrawRangeElementsBaseVertexOESFunPtr #-}

-- | Usage: @'glMultiDrawElementsBaseVertexOES' mode count type indices primcount basevertex@
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @count@ should be @COMPSIZE(drawcount)@.
--
-- The length of @indices@ should be @COMPSIZE(drawcount)@.
--
-- The length of @basevertex@ should be @COMPSIZE(drawcount)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiDrawElementsBaseVertex'.


glMultiDrawElementsBaseVertexOES :: MonadIO m => GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> m ()
glMultiDrawElementsBaseVertexOES = ffienumPtrsizeienumPtrPtrVsizeiPtrintIOV glMultiDrawElementsBaseVertexOESFunPtr

glMultiDrawElementsBaseVertexOESFunPtr :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> IO ())
glMultiDrawElementsBaseVertexOESFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsBaseVertexOES")

{-# NOINLINE glMultiDrawElementsBaseVertexOESFunPtr #-}