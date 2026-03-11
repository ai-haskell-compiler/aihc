-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DrawElementsBaseVertex (
  -- * Extension Support
    gl_EXT_draw_elements_base_vertex

  -- * GL_EXT_draw_elements_base_vertex
  , glDrawElementsBaseVertexEXT
  , glDrawElementsInstancedBaseVertexEXT
  , glDrawRangeElementsBaseVertexEXT
  , glMultiDrawElementsBaseVertexEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_EXT_draw_elements_base_vertex extension is available.

gl_EXT_draw_elements_base_vertex :: Bool
gl_EXT_draw_elements_base_vertex = member "GL_EXT_draw_elements_base_vertex" extensions
{-# NOINLINE gl_EXT_draw_elements_base_vertex #-}

-- | Usage: @'glDrawElementsBaseVertexEXT' mode count type indices basevertex@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsBaseVertex'.


glDrawElementsBaseVertexEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> m ()
glDrawElementsBaseVertexEXT = ffienumsizeienumPtrVintIOV glDrawElementsBaseVertexEXTFunPtr

glDrawElementsBaseVertexEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ())
glDrawElementsBaseVertexEXTFunPtr = unsafePerformIO (getProcAddress "glDrawElementsBaseVertexEXT")

{-# NOINLINE glDrawElementsBaseVertexEXTFunPtr #-}

-- | Usage: @'glDrawElementsInstancedBaseVertexEXT' mode count type indices instancecount basevertex@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsInstancedBaseVertex'.


glDrawElementsInstancedBaseVertexEXT :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> m ()
glDrawElementsInstancedBaseVertexEXT = ffienumsizeienumPtrVsizeiintIOV glDrawElementsInstancedBaseVertexEXTFunPtr

glDrawElementsInstancedBaseVertexEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> IO ())
glDrawElementsInstancedBaseVertexEXTFunPtr = unsafePerformIO (getProcAddress "glDrawElementsInstancedBaseVertexEXT")

{-# NOINLINE glDrawElementsInstancedBaseVertexEXTFunPtr #-}

-- | Usage: @'glDrawRangeElementsBaseVertexEXT' mode start end count type indices basevertex@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawRangeElementsBaseVertex'.


glDrawRangeElementsBaseVertexEXT :: MonadIO m => GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> m ()
glDrawRangeElementsBaseVertexEXT = ffienumuintuintsizeienumPtrVintIOV glDrawRangeElementsBaseVertexEXTFunPtr

glDrawRangeElementsBaseVertexEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ())
glDrawRangeElementsBaseVertexEXTFunPtr = unsafePerformIO (getProcAddress "glDrawRangeElementsBaseVertexEXT")

{-# NOINLINE glDrawRangeElementsBaseVertexEXTFunPtr #-}

-- | Usage: @'glMultiDrawElementsBaseVertexEXT' mode count type indices primcount basevertex@
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


glMultiDrawElementsBaseVertexEXT :: MonadIO m => GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> m ()
glMultiDrawElementsBaseVertexEXT = ffienumPtrsizeienumPtrPtrVsizeiPtrintIOV glMultiDrawElementsBaseVertexEXTFunPtr

glMultiDrawElementsBaseVertexEXTFunPtr :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> IO ())
glMultiDrawElementsBaseVertexEXTFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsBaseVertexEXT")

{-# NOINLINE glMultiDrawElementsBaseVertexEXTFunPtr #-}