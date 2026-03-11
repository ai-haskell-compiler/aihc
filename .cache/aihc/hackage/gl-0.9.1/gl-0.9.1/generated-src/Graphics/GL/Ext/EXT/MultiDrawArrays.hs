-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.MultiDrawArrays (
  -- * Extension Support
    gl_EXT_multi_draw_arrays

  -- * GL_EXT_multi_draw_arrays
  , glMultiDrawArraysEXT
  , glMultiDrawElementsEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/multi_draw_arrays.txt GL_EXT_multi_draw_arrays> extension is available.

gl_EXT_multi_draw_arrays :: Bool
gl_EXT_multi_draw_arrays = member "GL_EXT_multi_draw_arrays" extensions
{-# NOINLINE gl_EXT_multi_draw_arrays #-}

-- | Usage: @'glMultiDrawArraysEXT' mode first count primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The length of @first@ should be @COMPSIZE(primcount)@.
--
-- The length of @count@ should be @COMPSIZE(primcount)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiDrawArrays'.


glMultiDrawArraysEXT :: MonadIO m => GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> m ()
glMultiDrawArraysEXT = ffienumPtrintPtrsizeisizeiIOV glMultiDrawArraysEXTFunPtr

glMultiDrawArraysEXTFunPtr :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
glMultiDrawArraysEXTFunPtr = unsafePerformIO (getProcAddress "glMultiDrawArraysEXT")

{-# NOINLINE glMultiDrawArraysEXTFunPtr #-}

-- | Usage: @'glMultiDrawElementsEXT' mode count type indices primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @count@ should be @COMPSIZE(primcount)@.
--
-- The length of @indices@ should be @COMPSIZE(primcount)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiDrawElements'.


glMultiDrawElementsEXT :: MonadIO m => GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> m ()
glMultiDrawElementsEXT = ffienumPtrsizeienumPtrPtrVsizeiIOV glMultiDrawElementsEXTFunPtr

glMultiDrawElementsEXTFunPtr :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> IO ())
glMultiDrawElementsEXTFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsEXT")

{-# NOINLINE glMultiDrawElementsEXTFunPtr #-}