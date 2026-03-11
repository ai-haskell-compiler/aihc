-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.InstancedArrays (
  -- * Extension Support
    gl_ANGLE_instanced_arrays

  -- * GL_ANGLE_instanced_arrays
  , glDrawArraysInstancedANGLE
  , glDrawElementsInstancedANGLE
  , glVertexAttribDivisorANGLE
  , pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_instanced_arrays.txt GL_ANGLE_instanced_arrays> extension is available.

gl_ANGLE_instanced_arrays :: Bool
gl_ANGLE_instanced_arrays = member "GL_ANGLE_instanced_arrays" extensions
{-# NOINLINE gl_ANGLE_instanced_arrays #-}

-- | Usage: @'glDrawArraysInstancedANGLE' mode first count primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawArraysInstanced'.


glDrawArraysInstancedANGLE :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> m ()
glDrawArraysInstancedANGLE = ffienumintsizeisizeiIOV glDrawArraysInstancedANGLEFunPtr

glDrawArraysInstancedANGLEFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
glDrawArraysInstancedANGLEFunPtr = unsafePerformIO (getProcAddress "glDrawArraysInstancedANGLE")

{-# NOINLINE glDrawArraysInstancedANGLEFunPtr #-}

-- | Usage: @'glDrawElementsInstancedANGLE' mode count type indices primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsInstanced'.


glDrawElementsInstancedANGLE :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> m ()
glDrawElementsInstancedANGLE = ffienumsizeienumPtrVsizeiIOV glDrawElementsInstancedANGLEFunPtr

glDrawElementsInstancedANGLEFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> IO ())
glDrawElementsInstancedANGLEFunPtr = unsafePerformIO (getProcAddress "glDrawElementsInstancedANGLE")

{-# NOINLINE glDrawElementsInstancedANGLEFunPtr #-}

-- | Usage: @'glVertexAttribDivisorANGLE' index divisor@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribDivisor'.


glVertexAttribDivisorANGLE :: MonadIO m => GLuint -> GLuint -> m ()
glVertexAttribDivisorANGLE = ffiuintuintIOV glVertexAttribDivisorANGLEFunPtr

glVertexAttribDivisorANGLEFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glVertexAttribDivisorANGLEFunPtr = unsafePerformIO (getProcAddress "glVertexAttribDivisorANGLE")

{-# NOINLINE glVertexAttribDivisorANGLEFunPtr #-}

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE = 0x88FE