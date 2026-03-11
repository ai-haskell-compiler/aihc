-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DrawInstanced (
  -- * Extension Support
    gl_ARB_draw_instanced

  -- * GL_ARB_draw_instanced
  , glDrawArraysInstancedARB
  , glDrawElementsInstancedARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/draw_instanced.txt GL_ARB_draw_instanced> extension is available.

gl_ARB_draw_instanced :: Bool
gl_ARB_draw_instanced = member "GL_ARB_draw_instanced" extensions
{-# NOINLINE gl_ARB_draw_instanced #-}

-- | Usage: @'glDrawArraysInstancedARB' mode first count primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawArraysInstanced'.


glDrawArraysInstancedARB :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> m ()
glDrawArraysInstancedARB = ffienumintsizeisizeiIOV glDrawArraysInstancedARBFunPtr

glDrawArraysInstancedARBFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
glDrawArraysInstancedARBFunPtr = unsafePerformIO (getProcAddress "glDrawArraysInstancedARB")

{-# NOINLINE glDrawArraysInstancedARBFunPtr #-}

-- | Usage: @'glDrawElementsInstancedARB' mode count type indices primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The parameter @type@ is a @DrawElementsType@.
--
-- The length of @indices@ should be @COMPSIZE(count,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawElementsInstanced'.


glDrawElementsInstancedARB :: MonadIO m => GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> m ()
glDrawElementsInstancedARB = ffienumsizeienumPtrVsizeiIOV glDrawElementsInstancedARBFunPtr

glDrawElementsInstancedARBFunPtr :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> IO ())
glDrawElementsInstancedARBFunPtr = unsafePerformIO (getProcAddress "glDrawElementsInstancedARB")

{-# NOINLINE glDrawElementsInstancedARBFunPtr #-}