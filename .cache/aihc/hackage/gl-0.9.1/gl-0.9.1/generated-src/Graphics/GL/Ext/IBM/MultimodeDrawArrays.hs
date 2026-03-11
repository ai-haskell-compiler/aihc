-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IBM.MultimodeDrawArrays (
  -- * Extension Support
    gl_IBM_multimode_draw_arrays

  -- * GL_IBM_multimode_draw_arrays
  , glMultiModeDrawArraysIBM
  , glMultiModeDrawElementsIBM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/IBM/multimode_draw_arrays.txt GL_IBM_multimode_draw_arrays> extension is available.

gl_IBM_multimode_draw_arrays :: Bool
gl_IBM_multimode_draw_arrays = member "GL_IBM_multimode_draw_arrays" extensions
{-# NOINLINE gl_IBM_multimode_draw_arrays #-}

-- | Usage: @'glMultiModeDrawArraysIBM' mode first count primcount modestride@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The length of @mode@ should be @COMPSIZE(primcount)@.
--
-- The length of @first@ should be @COMPSIZE(primcount)@.
--
-- The length of @count@ should be @COMPSIZE(primcount)@.


glMultiModeDrawArraysIBM :: MonadIO m => Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> m ()
glMultiModeDrawArraysIBM = ffiPtrenumPtrintPtrsizeisizeiintIOV glMultiModeDrawArraysIBMFunPtr

glMultiModeDrawArraysIBMFunPtr :: FunPtr (Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> IO ())
glMultiModeDrawArraysIBMFunPtr = unsafePerformIO (getProcAddress "glMultiModeDrawArraysIBM")

{-# NOINLINE glMultiModeDrawArraysIBMFunPtr #-}

-- | Usage: @'glMultiModeDrawElementsIBM' mode count type indices primcount modestride@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @mode@ should be @COMPSIZE(primcount)@.
--
-- The length of @count@ should be @COMPSIZE(primcount)@.
--
-- The length of @indices@ should be @COMPSIZE(primcount)@.


glMultiModeDrawElementsIBM :: MonadIO m => Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GLint -> m ()
glMultiModeDrawElementsIBM = ffiPtrenumPtrsizeienumPtrPtrVsizeiintIOV glMultiModeDrawElementsIBMFunPtr

glMultiModeDrawElementsIBMFunPtr :: FunPtr (Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GLint -> IO ())
glMultiModeDrawElementsIBMFunPtr = unsafePerformIO (getProcAddress "glMultiModeDrawElementsIBM")

{-# NOINLINE glMultiModeDrawElementsIBMFunPtr #-}