-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.VertexArray (
  -- * Extension Support
    gl_EXT_vertex_array

  -- * GL_EXT_vertex_array
  , glArrayElementEXT
  , glColorPointerEXT
  , glDrawArraysEXT
  , glEdgeFlagPointerEXT
  , glGetPointervEXT
  , glIndexPointerEXT
  , glNormalPointerEXT
  , glTexCoordPointerEXT
  , glVertexPointerEXT
  , pattern GL_COLOR_ARRAY_COUNT_EXT
  , pattern GL_COLOR_ARRAY_EXT
  , pattern GL_COLOR_ARRAY_POINTER_EXT
  , pattern GL_COLOR_ARRAY_SIZE_EXT
  , pattern GL_COLOR_ARRAY_STRIDE_EXT
  , pattern GL_COLOR_ARRAY_TYPE_EXT
  , pattern GL_EDGE_FLAG_ARRAY_COUNT_EXT
  , pattern GL_EDGE_FLAG_ARRAY_EXT
  , pattern GL_EDGE_FLAG_ARRAY_POINTER_EXT
  , pattern GL_EDGE_FLAG_ARRAY_STRIDE_EXT
  , pattern GL_INDEX_ARRAY_COUNT_EXT
  , pattern GL_INDEX_ARRAY_EXT
  , pattern GL_INDEX_ARRAY_POINTER_EXT
  , pattern GL_INDEX_ARRAY_STRIDE_EXT
  , pattern GL_INDEX_ARRAY_TYPE_EXT
  , pattern GL_NORMAL_ARRAY_COUNT_EXT
  , pattern GL_NORMAL_ARRAY_EXT
  , pattern GL_NORMAL_ARRAY_POINTER_EXT
  , pattern GL_NORMAL_ARRAY_STRIDE_EXT
  , pattern GL_NORMAL_ARRAY_TYPE_EXT
  , pattern GL_TEXTURE_COORD_ARRAY_COUNT_EXT
  , pattern GL_TEXTURE_COORD_ARRAY_EXT
  , pattern GL_TEXTURE_COORD_ARRAY_POINTER_EXT
  , pattern GL_TEXTURE_COORD_ARRAY_SIZE_EXT
  , pattern GL_TEXTURE_COORD_ARRAY_STRIDE_EXT
  , pattern GL_TEXTURE_COORD_ARRAY_TYPE_EXT
  , pattern GL_VERTEX_ARRAY_COUNT_EXT
  , pattern GL_VERTEX_ARRAY_EXT
  , pattern GL_VERTEX_ARRAY_POINTER_EXT
  , pattern GL_VERTEX_ARRAY_SIZE_EXT
  , pattern GL_VERTEX_ARRAY_STRIDE_EXT
  , pattern GL_VERTEX_ARRAY_TYPE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/vertex_array.txt GL_EXT_vertex_array> extension is available.

gl_EXT_vertex_array :: Bool
gl_EXT_vertex_array = member "GL_EXT_vertex_array" extensions
{-# NOINLINE gl_EXT_vertex_array #-}

-- | Usage: @'glArrayElementEXT' i@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glArrayElement'.


glArrayElementEXT :: MonadIO m => GLint -> m ()
glArrayElementEXT = ffiintIOV glArrayElementEXTFunPtr

glArrayElementEXTFunPtr :: FunPtr (GLint -> IO ())
glArrayElementEXTFunPtr = unsafePerformIO (getProcAddress "glArrayElementEXT")

{-# NOINLINE glArrayElementEXTFunPtr #-}

-- | Usage: @'glColorPointerEXT' size type stride count pointer@
--
-- The parameter @type@ is a @ColorPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride,count)@.


glColorPointerEXT :: MonadIO m => GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> m ()
glColorPointerEXT = ffiintenumsizeisizeiPtrVIOV glColorPointerEXTFunPtr

glColorPointerEXTFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ())
glColorPointerEXTFunPtr = unsafePerformIO (getProcAddress "glColorPointerEXT")

{-# NOINLINE glColorPointerEXTFunPtr #-}

-- | Usage: @'glDrawArraysEXT' mode first count@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawArrays'.


glDrawArraysEXT :: MonadIO m => GLenum -> GLint -> GLsizei -> m ()
glDrawArraysEXT = ffienumintsizeiIOV glDrawArraysEXTFunPtr

glDrawArraysEXTFunPtr :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
glDrawArraysEXTFunPtr = unsafePerformIO (getProcAddress "glDrawArraysEXT")

{-# NOINLINE glDrawArraysEXTFunPtr #-}

-- | Usage: @'glEdgeFlagPointerEXT' stride count pointer@
--
-- The parameter @pointer@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @pointer@ should be @COMPSIZE(stride,count)@.


glEdgeFlagPointerEXT :: MonadIO m => GLsizei -> GLsizei -> Ptr GLboolean -> m ()
glEdgeFlagPointerEXT = ffisizeisizeiPtrbooleanIOV glEdgeFlagPointerEXTFunPtr

glEdgeFlagPointerEXTFunPtr :: FunPtr (GLsizei -> GLsizei -> Ptr GLboolean -> IO ())
glEdgeFlagPointerEXTFunPtr = unsafePerformIO (getProcAddress "glEdgeFlagPointerEXT")

{-# NOINLINE glEdgeFlagPointerEXTFunPtr #-}

-- | Usage: @'glGetPointervEXT' pname params@
--
-- The parameter @pname@ is a @GetPointervPName@, one of: 'Graphics.GL.Internal.Shared.GL_COLOR_ARRAY_POINTER', 'Graphics.GL.Ext.EXT.VertexArray.GL_COLOR_ARRAY_POINTER_EXT', 'Graphics.GL.Internal.Shared.GL_EDGE_FLAG_ARRAY_POINTER', 'Graphics.GL.Ext.EXT.VertexArray.GL_EDGE_FLAG_ARRAY_POINTER_EXT', 'Graphics.GL.Internal.Shared.GL_FEEDBACK_BUFFER_POINTER', 'Graphics.GL.Internal.Shared.GL_INDEX_ARRAY_POINTER', 'Graphics.GL.Ext.EXT.VertexArray.GL_INDEX_ARRAY_POINTER_EXT', 'Graphics.GL.Ext.SGIX.Instruments.GL_INSTRUMENT_BUFFER_POINTER_SGIX', 'Graphics.GL.Internal.Shared.GL_NORMAL_ARRAY_POINTER', 'Graphics.GL.Ext.EXT.VertexArray.GL_NORMAL_ARRAY_POINTER_EXT', 'Graphics.GL.Internal.Shared.GL_SELECTION_BUFFER_POINTER', 'Graphics.GL.Internal.Shared.GL_TEXTURE_COORD_ARRAY_POINTER', 'Graphics.GL.Ext.EXT.VertexArray.GL_TEXTURE_COORD_ARRAY_POINTER_EXT', 'Graphics.GL.Internal.Shared.GL_VERTEX_ARRAY_POINTER', 'Graphics.GL.Ext.EXT.VertexArray.GL_VERTEX_ARRAY_POINTER_EXT'.
--
-- The length of @params@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetPointerv'.


glGetPointervEXT :: MonadIO m => GLenum -> Ptr (Ptr ()) -> m ()
glGetPointervEXT = ffienumPtrPtrVIOV glGetPointervEXTFunPtr

glGetPointervEXTFunPtr :: FunPtr (GLenum -> Ptr (Ptr ()) -> IO ())
glGetPointervEXTFunPtr = unsafePerformIO (getProcAddress "glGetPointervEXT")

{-# NOINLINE glGetPointervEXTFunPtr #-}

-- | Usage: @'glIndexPointerEXT' type stride count pointer@
--
-- The parameter @type@ is a @IndexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride,count)@.


glIndexPointerEXT :: MonadIO m => GLenum -> GLsizei -> GLsizei -> Ptr () -> m ()
glIndexPointerEXT = ffienumsizeisizeiPtrVIOV glIndexPointerEXTFunPtr

glIndexPointerEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ())
glIndexPointerEXTFunPtr = unsafePerformIO (getProcAddress "glIndexPointerEXT")

{-# NOINLINE glIndexPointerEXTFunPtr #-}

-- | Usage: @'glNormalPointerEXT' type stride count pointer@
--
-- The parameter @type@ is a @NormalPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride,count)@.


glNormalPointerEXT :: MonadIO m => GLenum -> GLsizei -> GLsizei -> Ptr () -> m ()
glNormalPointerEXT = ffienumsizeisizeiPtrVIOV glNormalPointerEXTFunPtr

glNormalPointerEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ())
glNormalPointerEXTFunPtr = unsafePerformIO (getProcAddress "glNormalPointerEXT")

{-# NOINLINE glNormalPointerEXTFunPtr #-}

-- | Usage: @'glTexCoordPointerEXT' size type stride count pointer@
--
-- The parameter @type@ is a @TexCoordPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride,count)@.


glTexCoordPointerEXT :: MonadIO m => GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> m ()
glTexCoordPointerEXT = ffiintenumsizeisizeiPtrVIOV glTexCoordPointerEXTFunPtr

glTexCoordPointerEXTFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ())
glTexCoordPointerEXTFunPtr = unsafePerformIO (getProcAddress "glTexCoordPointerEXT")

{-# NOINLINE glTexCoordPointerEXTFunPtr #-}

-- | Usage: @'glVertexPointerEXT' size type stride count pointer@
--
-- The parameter @type@ is a @VertexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride,count)@.


glVertexPointerEXT :: MonadIO m => GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> m ()
glVertexPointerEXT = ffiintenumsizeisizeiPtrVIOV glVertexPointerEXTFunPtr

glVertexPointerEXTFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ())
glVertexPointerEXTFunPtr = unsafePerformIO (getProcAddress "glVertexPointerEXT")

{-# NOINLINE glVertexPointerEXTFunPtr #-}

pattern GL_COLOR_ARRAY_COUNT_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_COUNT_EXT = 0x8084

pattern GL_COLOR_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_EXT = 0x8076

pattern GL_COLOR_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_POINTER_EXT = 0x8090

pattern GL_COLOR_ARRAY_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_SIZE_EXT = 0x8081

pattern GL_COLOR_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_STRIDE_EXT = 0x8083

pattern GL_COLOR_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_TYPE_EXT = 0x8082

pattern GL_EDGE_FLAG_ARRAY_COUNT_EXT  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_COUNT_EXT = 0x808D

pattern GL_EDGE_FLAG_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_EXT = 0x8079

pattern GL_EDGE_FLAG_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_POINTER_EXT = 0x8093

pattern GL_EDGE_FLAG_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_STRIDE_EXT = 0x808C

pattern GL_INDEX_ARRAY_COUNT_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_COUNT_EXT = 0x8087

pattern GL_INDEX_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_EXT = 0x8077

pattern GL_INDEX_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_POINTER_EXT = 0x8091

pattern GL_INDEX_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_STRIDE_EXT = 0x8086

pattern GL_INDEX_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_TYPE_EXT = 0x8085

pattern GL_NORMAL_ARRAY_COUNT_EXT  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_COUNT_EXT = 0x8080

pattern GL_NORMAL_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_EXT = 0x8075

pattern GL_NORMAL_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_POINTER_EXT = 0x808F

pattern GL_NORMAL_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_STRIDE_EXT = 0x807F

pattern GL_NORMAL_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_TYPE_EXT = 0x807E

pattern GL_TEXTURE_COORD_ARRAY_COUNT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_COUNT_EXT = 0x808B

pattern GL_TEXTURE_COORD_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_EXT = 0x8078

pattern GL_TEXTURE_COORD_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_POINTER_EXT = 0x8092

pattern GL_TEXTURE_COORD_ARRAY_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_SIZE_EXT = 0x8088

pattern GL_TEXTURE_COORD_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = 0x808A

pattern GL_TEXTURE_COORD_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_TYPE_EXT = 0x8089

pattern GL_VERTEX_ARRAY_COUNT_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_COUNT_EXT = 0x807D

pattern GL_VERTEX_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_EXT = 0x8074

pattern GL_VERTEX_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_POINTER_EXT = 0x808E

pattern GL_VERTEX_ARRAY_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_SIZE_EXT = 0x807A

pattern GL_VERTEX_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_STRIDE_EXT = 0x807C

pattern GL_VERTEX_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_TYPE_EXT = 0x807B