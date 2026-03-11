-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexBufferObject (
  -- * Extension Support
    gl_ARB_vertex_buffer_object

  -- * GL_ARB_vertex_buffer_object
  , glBindBufferARB
  , glBufferDataARB
  , glBufferSubDataARB
  , glDeleteBuffersARB
  , glGenBuffersARB
  , glGetBufferParameterivARB
  , glGetBufferPointervARB
  , glGetBufferSubDataARB
  , glIsBufferARB
  , glMapBufferARB
  , glUnmapBufferARB
  , pattern GL_ARRAY_BUFFER_ARB
  , pattern GL_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_BUFFER_ACCESS_ARB
  , pattern GL_BUFFER_MAPPED_ARB
  , pattern GL_BUFFER_MAP_POINTER_ARB
  , pattern GL_BUFFER_SIZE_ARB
  , pattern GL_BUFFER_USAGE_ARB
  , pattern GL_COLOR_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_DYNAMIC_COPY_ARB
  , pattern GL_DYNAMIC_DRAW_ARB
  , pattern GL_DYNAMIC_READ_ARB
  , pattern GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_ELEMENT_ARRAY_BUFFER_ARB
  , pattern GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_INDEX_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_NORMAL_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_READ_ONLY_ARB
  , pattern GL_READ_WRITE_ARB
  , pattern GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_STATIC_COPY_ARB
  , pattern GL_STATIC_DRAW_ARB
  , pattern GL_STATIC_READ_ARB
  , pattern GL_STREAM_COPY_ARB
  , pattern GL_STREAM_DRAW_ARB
  , pattern GL_STREAM_READ_ARB
  , pattern GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_VERTEX_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB
  , pattern GL_WRITE_ONLY_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_buffer_object.txt GL_ARB_vertex_buffer_object> extension is available.

gl_ARB_vertex_buffer_object :: Bool
gl_ARB_vertex_buffer_object = member "GL_ARB_vertex_buffer_object" extensions
{-# NOINLINE gl_ARB_vertex_buffer_object #-}

-- | Usage: @'glBindBufferARB' target buffer@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindBuffer'.


glBindBufferARB :: MonadIO m => GLenum -> GLuint -> m ()
glBindBufferARB = ffienumuintIOV glBindBufferARBFunPtr

glBindBufferARBFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindBufferARBFunPtr = unsafePerformIO (getProcAddress "glBindBufferARB")

{-# NOINLINE glBindBufferARBFunPtr #-}

-- | Usage: @'glBufferDataARB' target size data usage@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- The parameter @size@ is a @BufferSizeARB@.
--
-- The parameter @usage@ is a @BufferUsageARB@.
--
-- The length of @data@ should be @size@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBufferData'.


glBufferDataARB :: MonadIO m => GLenum -> GLsizeiptrARB -> Ptr () -> GLenum -> m ()
glBufferDataARB = ffienumsizeiptrARBPtrVenumIOV glBufferDataARBFunPtr

glBufferDataARBFunPtr :: FunPtr (GLenum -> GLsizeiptrARB -> Ptr () -> GLenum -> IO ())
glBufferDataARBFunPtr = unsafePerformIO (getProcAddress "glBufferDataARB")

{-# NOINLINE glBufferDataARBFunPtr #-}

-- | Usage: @'glBufferSubDataARB' target offset size data@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- The parameter @offset@ is a @BufferOffsetARB@.
--
-- The parameter @size@ is a @BufferSizeARB@.
--
-- The length of @data@ should be @size@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBufferSubData'.


glBufferSubDataARB :: MonadIO m => GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> m ()
glBufferSubDataARB = ffienumintptrARBsizeiptrARBPtrVIOV glBufferSubDataARBFunPtr

glBufferSubDataARBFunPtr :: FunPtr (GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> IO ())
glBufferSubDataARBFunPtr = unsafePerformIO (getProcAddress "glBufferSubDataARB")

{-# NOINLINE glBufferSubDataARBFunPtr #-}

-- | Usage: @'glDeleteBuffersARB' n buffers@
--
-- The length of @buffers@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteBuffers'.


glDeleteBuffersARB :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteBuffersARB = ffisizeiPtruintIOV glDeleteBuffersARBFunPtr

glDeleteBuffersARBFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteBuffersARBFunPtr = unsafePerformIO (getProcAddress "glDeleteBuffersARB")

{-# NOINLINE glDeleteBuffersARBFunPtr #-}

-- | Usage: @'glGenBuffersARB' n buffers@
--
-- The length of @buffers@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenBuffers'.


glGenBuffersARB :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenBuffersARB = ffisizeiPtruintIOV glGenBuffersARBFunPtr

glGenBuffersARBFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenBuffersARBFunPtr = unsafePerformIO (getProcAddress "glGenBuffersARB")

{-# NOINLINE glGenBuffersARBFunPtr #-}

-- | Usage: @'glGetBufferParameterivARB' target pname params@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- The parameter @pname@ is a @BufferPNameARB@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetBufferParameteriv'.


glGetBufferParameterivARB :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetBufferParameterivARB = ffienumenumPtrintIOV glGetBufferParameterivARBFunPtr

glGetBufferParameterivARBFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetBufferParameterivARBFunPtr = unsafePerformIO (getProcAddress "glGetBufferParameterivARB")

{-# NOINLINE glGetBufferParameterivARBFunPtr #-}

-- | Usage: @'glGetBufferPointervARB' target pname params@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- The parameter @pname@ is a @BufferPointerNameARB@.
--
-- The length of @params@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetBufferPointerv'.


glGetBufferPointervARB :: MonadIO m => GLenum -> GLenum -> Ptr (Ptr ()) -> m ()
glGetBufferPointervARB = ffienumenumPtrPtrVIOV glGetBufferPointervARBFunPtr

glGetBufferPointervARBFunPtr :: FunPtr (GLenum -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetBufferPointervARBFunPtr = unsafePerformIO (getProcAddress "glGetBufferPointervARB")

{-# NOINLINE glGetBufferPointervARBFunPtr #-}

-- | Usage: @'glGetBufferSubDataARB' target offset size data@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- The parameter @offset@ is a @BufferOffsetARB@.
--
-- The parameter @size@ is a @BufferSizeARB@.
--
-- The length of @data@ should be @size@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetBufferSubData'.


glGetBufferSubDataARB :: MonadIO m => GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> m ()
glGetBufferSubDataARB = ffienumintptrARBsizeiptrARBPtrVIOV glGetBufferSubDataARBFunPtr

glGetBufferSubDataARBFunPtr :: FunPtr (GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> IO ())
glGetBufferSubDataARBFunPtr = unsafePerformIO (getProcAddress "glGetBufferSubDataARB")

{-# NOINLINE glGetBufferSubDataARBFunPtr #-}

-- | Usage: @'glIsBufferARB' buffer@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsBuffer'.


glIsBufferARB :: MonadIO m => GLuint -> m GLboolean
glIsBufferARB = ffiuintIOboolean glIsBufferARBFunPtr

glIsBufferARBFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsBufferARBFunPtr = unsafePerformIO (getProcAddress "glIsBufferARB")

{-# NOINLINE glIsBufferARBFunPtr #-}

-- | Usage: @'glMapBufferARB' target access@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- The parameter @access@ is a @BufferAccessARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMapBuffer'.


glMapBufferARB :: MonadIO m => GLenum -> GLenum -> m (Ptr ())
glMapBufferARB = ffienumenumIOPtrV glMapBufferARBFunPtr

glMapBufferARBFunPtr :: FunPtr (GLenum -> GLenum -> IO (Ptr ()))
glMapBufferARBFunPtr = unsafePerformIO (getProcAddress "glMapBufferARB")

{-# NOINLINE glMapBufferARBFunPtr #-}

-- | Usage: @'glUnmapBufferARB' target@
--
-- The parameter @target@ is a @BufferTargetARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUnmapBuffer'.


glUnmapBufferARB :: MonadIO m => GLenum -> m GLboolean
glUnmapBufferARB = ffienumIOboolean glUnmapBufferARBFunPtr

glUnmapBufferARBFunPtr :: FunPtr (GLenum -> IO GLboolean)
glUnmapBufferARBFunPtr = unsafePerformIO (getProcAddress "glUnmapBufferARB")

{-# NOINLINE glUnmapBufferARBFunPtr #-}

pattern GL_ARRAY_BUFFER_ARB  :: (Eq a, Num a) => a

pattern GL_ARRAY_BUFFER_ARB = 0x8892

pattern GL_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_ARRAY_BUFFER_BINDING_ARB = 0x8894

pattern GL_BUFFER_ACCESS_ARB  :: (Eq a, Num a) => a

pattern GL_BUFFER_ACCESS_ARB = 0x88BB

pattern GL_BUFFER_MAPPED_ARB  :: (Eq a, Num a) => a

pattern GL_BUFFER_MAPPED_ARB = 0x88BC

pattern GL_BUFFER_MAP_POINTER_ARB  :: (Eq a, Num a) => a

pattern GL_BUFFER_MAP_POINTER_ARB = 0x88BD

pattern GL_BUFFER_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_BUFFER_SIZE_ARB = 0x8764

pattern GL_BUFFER_USAGE_ARB  :: (Eq a, Num a) => a

pattern GL_BUFFER_USAGE_ARB = 0x8765

pattern GL_COLOR_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_BUFFER_BINDING_ARB = 0x8898

pattern GL_DYNAMIC_COPY_ARB  :: (Eq a, Num a) => a

pattern GL_DYNAMIC_COPY_ARB = 0x88EA

pattern GL_DYNAMIC_DRAW_ARB  :: (Eq a, Num a) => a

pattern GL_DYNAMIC_DRAW_ARB = 0x88E8

pattern GL_DYNAMIC_READ_ARB  :: (Eq a, Num a) => a

pattern GL_DYNAMIC_READ_ARB = 0x88E9

pattern GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB = 0x889B

pattern GL_ELEMENT_ARRAY_BUFFER_ARB  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_BUFFER_ARB = 0x8893

pattern GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB = 0x8895

pattern GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB = 0x889D

pattern GL_INDEX_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_BUFFER_BINDING_ARB = 0x8899

pattern GL_NORMAL_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_BUFFER_BINDING_ARB = 0x8897

pattern GL_READ_ONLY_ARB  :: (Eq a, Num a) => a

pattern GL_READ_ONLY_ARB = 0x88B8

pattern GL_READ_WRITE_ARB  :: (Eq a, Num a) => a

pattern GL_READ_WRITE_ARB = 0x88BA

pattern GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB = 0x889C

pattern GL_STATIC_COPY_ARB  :: (Eq a, Num a) => a

pattern GL_STATIC_COPY_ARB = 0x88E6

pattern GL_STATIC_DRAW_ARB  :: (Eq a, Num a) => a

pattern GL_STATIC_DRAW_ARB = 0x88E4

pattern GL_STATIC_READ_ARB  :: (Eq a, Num a) => a

pattern GL_STATIC_READ_ARB = 0x88E5

pattern GL_STREAM_COPY_ARB  :: (Eq a, Num a) => a

pattern GL_STREAM_COPY_ARB = 0x88E2

pattern GL_STREAM_DRAW_ARB  :: (Eq a, Num a) => a

pattern GL_STREAM_DRAW_ARB = 0x88E0

pattern GL_STREAM_READ_ARB  :: (Eq a, Num a) => a

pattern GL_STREAM_READ_ARB = 0x88E1

pattern GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB = 0x889A

pattern GL_VERTEX_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_BUFFER_BINDING_ARB = 0x8896

pattern GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB = 0x889F

pattern GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB = 0x889E

pattern GL_WRITE_ONLY_ARB  :: (Eq a, Num a) => a

pattern GL_WRITE_ONLY_ARB = 0x88B9