-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.ElementArray (
  -- * Extension Support
    gl_ATI_element_array

  -- * GL_ATI_element_array
  , glDrawElementArrayATI
  , glDrawRangeElementArrayATI
  , glElementPointerATI
  , pattern GL_ELEMENT_ARRAY_ATI
  , pattern GL_ELEMENT_ARRAY_POINTER_ATI
  , pattern GL_ELEMENT_ARRAY_TYPE_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/element_array.txt GL_ATI_element_array> extension is available.

gl_ATI_element_array :: Bool
gl_ATI_element_array = member "GL_ATI_element_array" extensions
{-# NOINLINE gl_ATI_element_array #-}

-- | Usage: @'glDrawElementArrayATI' mode count@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.


glDrawElementArrayATI :: MonadIO m => GLenum -> GLsizei -> m ()
glDrawElementArrayATI = ffienumsizeiIOV glDrawElementArrayATIFunPtr

glDrawElementArrayATIFunPtr :: FunPtr (GLenum -> GLsizei -> IO ())
glDrawElementArrayATIFunPtr = unsafePerformIO (getProcAddress "glDrawElementArrayATI")

{-# NOINLINE glDrawElementArrayATIFunPtr #-}

-- | Usage: @'glDrawRangeElementArrayATI' mode start end count@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.


glDrawRangeElementArrayATI :: MonadIO m => GLenum -> GLuint -> GLuint -> GLsizei -> m ()
glDrawRangeElementArrayATI = ffienumuintuintsizeiIOV glDrawRangeElementArrayATIFunPtr

glDrawRangeElementArrayATIFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> IO ())
glDrawRangeElementArrayATIFunPtr = unsafePerformIO (getProcAddress "glDrawRangeElementArrayATI")

{-# NOINLINE glDrawRangeElementArrayATIFunPtr #-}

-- | Usage: @'glElementPointerATI' type pointer@
--
-- The parameter @type@ is a @ElementPointerTypeATI@.
--
-- The length of @pointer@ should be @COMPSIZE(type)@.


glElementPointerATI :: MonadIO m => GLenum -> Ptr () -> m ()
glElementPointerATI = ffienumPtrVIOV glElementPointerATIFunPtr

glElementPointerATIFunPtr :: FunPtr (GLenum -> Ptr () -> IO ())
glElementPointerATIFunPtr = unsafePerformIO (getProcAddress "glElementPointerATI")

{-# NOINLINE glElementPointerATIFunPtr #-}

pattern GL_ELEMENT_ARRAY_ATI  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_ATI = 0x8768

pattern GL_ELEMENT_ARRAY_POINTER_ATI  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_POINTER_ATI = 0x876A

pattern GL_ELEMENT_ARRAY_TYPE_ATI  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_TYPE_ATI = 0x8769