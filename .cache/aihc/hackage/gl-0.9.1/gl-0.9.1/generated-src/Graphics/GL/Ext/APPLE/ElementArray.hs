-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.ElementArray (
  -- * Extension Support
    gl_APPLE_element_array

  -- * GL_APPLE_element_array
  , glDrawElementArrayAPPLE
  , glDrawRangeElementArrayAPPLE
  , glElementPointerAPPLE
  , glMultiDrawElementArrayAPPLE
  , glMultiDrawRangeElementArrayAPPLE
  , pattern GL_ELEMENT_ARRAY_APPLE
  , pattern GL_ELEMENT_ARRAY_POINTER_APPLE
  , pattern GL_ELEMENT_ARRAY_TYPE_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/element_array.txt GL_APPLE_element_array> extension is available.

gl_APPLE_element_array :: Bool
gl_APPLE_element_array = member "GL_APPLE_element_array" extensions
{-# NOINLINE gl_APPLE_element_array #-}

-- | Usage: @'glDrawElementArrayAPPLE' mode first count@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.


glDrawElementArrayAPPLE :: MonadIO m => GLenum -> GLint -> GLsizei -> m ()
glDrawElementArrayAPPLE = ffienumintsizeiIOV glDrawElementArrayAPPLEFunPtr

glDrawElementArrayAPPLEFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
glDrawElementArrayAPPLEFunPtr = unsafePerformIO (getProcAddress "glDrawElementArrayAPPLE")

{-# NOINLINE glDrawElementArrayAPPLEFunPtr #-}

-- | Usage: @'glDrawRangeElementArrayAPPLE' mode start end first count@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.


glDrawRangeElementArrayAPPLE :: MonadIO m => GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> m ()
glDrawRangeElementArrayAPPLE = ffienumuintuintintsizeiIOV glDrawRangeElementArrayAPPLEFunPtr

glDrawRangeElementArrayAPPLEFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> IO ())
glDrawRangeElementArrayAPPLEFunPtr = unsafePerformIO (getProcAddress "glDrawRangeElementArrayAPPLE")

{-# NOINLINE glDrawRangeElementArrayAPPLEFunPtr #-}

-- | Usage: @'glElementPointerAPPLE' type pointer@
--
-- The parameter @type@ is a @ElementPointerTypeATI@.
--
-- The length of @pointer@ should be @COMPSIZE(type)@.


glElementPointerAPPLE :: MonadIO m => GLenum -> Ptr () -> m ()
glElementPointerAPPLE = ffienumPtrVIOV glElementPointerAPPLEFunPtr

glElementPointerAPPLEFunPtr :: FunPtr (GLenum -> Ptr () -> IO ())
glElementPointerAPPLEFunPtr = unsafePerformIO (getProcAddress "glElementPointerAPPLE")

{-# NOINLINE glElementPointerAPPLEFunPtr #-}

-- | Usage: @'glMultiDrawElementArrayAPPLE' mode first count primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The length of @first@ should be @primcount@.
--
-- The length of @count@ should be @primcount@.


glMultiDrawElementArrayAPPLE :: MonadIO m => GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> m ()
glMultiDrawElementArrayAPPLE = ffienumPtrintPtrsizeisizeiIOV glMultiDrawElementArrayAPPLEFunPtr

glMultiDrawElementArrayAPPLEFunPtr :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
glMultiDrawElementArrayAPPLEFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementArrayAPPLE")

{-# NOINLINE glMultiDrawElementArrayAPPLEFunPtr #-}

-- | Usage: @'glMultiDrawRangeElementArrayAPPLE' mode start end first count primcount@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- The length of @first@ should be @primcount@.
--
-- The length of @count@ should be @primcount@.


glMultiDrawRangeElementArrayAPPLE :: MonadIO m => GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> m ()
glMultiDrawRangeElementArrayAPPLE = ffienumuintuintPtrintPtrsizeisizeiIOV glMultiDrawRangeElementArrayAPPLEFunPtr

glMultiDrawRangeElementArrayAPPLEFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
glMultiDrawRangeElementArrayAPPLEFunPtr = unsafePerformIO (getProcAddress "glMultiDrawRangeElementArrayAPPLE")

{-# NOINLINE glMultiDrawRangeElementArrayAPPLEFunPtr #-}

pattern GL_ELEMENT_ARRAY_APPLE  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_APPLE = 0x8A0C

pattern GL_ELEMENT_ARRAY_POINTER_APPLE  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_POINTER_APPLE = 0x8A0E

pattern GL_ELEMENT_ARRAY_TYPE_APPLE  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_TYPE_APPLE = 0x8A0D