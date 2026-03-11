-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.VertexStreams (
  -- * Extension Support
    gl_ATI_vertex_streams

  -- * GL_ATI_vertex_streams
  , glClientActiveVertexStreamATI
  , glNormalStream3bATI
  , glNormalStream3bvATI
  , glNormalStream3dATI
  , glNormalStream3dvATI
  , glNormalStream3fATI
  , glNormalStream3fvATI
  , glNormalStream3iATI
  , glNormalStream3ivATI
  , glNormalStream3sATI
  , glNormalStream3svATI
  , glVertexBlendEnvfATI
  , glVertexBlendEnviATI
  , glVertexStream1dATI
  , glVertexStream1dvATI
  , glVertexStream1fATI
  , glVertexStream1fvATI
  , glVertexStream1iATI
  , glVertexStream1ivATI
  , glVertexStream1sATI
  , glVertexStream1svATI
  , glVertexStream2dATI
  , glVertexStream2dvATI
  , glVertexStream2fATI
  , glVertexStream2fvATI
  , glVertexStream2iATI
  , glVertexStream2ivATI
  , glVertexStream2sATI
  , glVertexStream2svATI
  , glVertexStream3dATI
  , glVertexStream3dvATI
  , glVertexStream3fATI
  , glVertexStream3fvATI
  , glVertexStream3iATI
  , glVertexStream3ivATI
  , glVertexStream3sATI
  , glVertexStream3svATI
  , glVertexStream4dATI
  , glVertexStream4dvATI
  , glVertexStream4fATI
  , glVertexStream4fvATI
  , glVertexStream4iATI
  , glVertexStream4ivATI
  , glVertexStream4sATI
  , glVertexStream4svATI
  , pattern GL_MAX_VERTEX_STREAMS_ATI
  , pattern GL_VERTEX_SOURCE_ATI
  , pattern GL_VERTEX_STREAM0_ATI
  , pattern GL_VERTEX_STREAM1_ATI
  , pattern GL_VERTEX_STREAM2_ATI
  , pattern GL_VERTEX_STREAM3_ATI
  , pattern GL_VERTEX_STREAM4_ATI
  , pattern GL_VERTEX_STREAM5_ATI
  , pattern GL_VERTEX_STREAM6_ATI
  , pattern GL_VERTEX_STREAM7_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/vertex_streams.txt GL_ATI_vertex_streams> extension is available.

gl_ATI_vertex_streams :: Bool
gl_ATI_vertex_streams = member "GL_ATI_vertex_streams" extensions
{-# NOINLINE gl_ATI_vertex_streams #-}

-- | Usage: @'glClientActiveVertexStreamATI' stream@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glClientActiveVertexStreamATI :: MonadIO m => GLenum -> m ()
glClientActiveVertexStreamATI = ffienumIOV glClientActiveVertexStreamATIFunPtr

glClientActiveVertexStreamATIFunPtr :: FunPtr (GLenum -> IO ())
glClientActiveVertexStreamATIFunPtr = unsafePerformIO (getProcAddress "glClientActiveVertexStreamATI")

{-# NOINLINE glClientActiveVertexStreamATIFunPtr #-}

-- | Usage: @'glNormalStream3bATI' stream nx ny nz@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glNormalStream3bATI :: MonadIO m => GLenum -> GLbyte -> GLbyte -> GLbyte -> m ()
glNormalStream3bATI = ffienumbytebytebyteIOV glNormalStream3bATIFunPtr

glNormalStream3bATIFunPtr :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ())
glNormalStream3bATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3bATI")

{-# NOINLINE glNormalStream3bATIFunPtr #-}

-- | Usage: @'glNormalStream3bvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glNormalStream3bvATI :: MonadIO m => GLenum -> Ptr GLbyte -> m ()
glNormalStream3bvATI = ffienumPtrbyteIOV glNormalStream3bvATIFunPtr

glNormalStream3bvATIFunPtr :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
glNormalStream3bvATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3bvATI")

{-# NOINLINE glNormalStream3bvATIFunPtr #-}

-- | Usage: @'glNormalStream3dATI' stream nx ny nz@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glNormalStream3dATI :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> m ()
glNormalStream3dATI = ffienumdoubledoubledoubleIOV glNormalStream3dATIFunPtr

glNormalStream3dATIFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
glNormalStream3dATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3dATI")

{-# NOINLINE glNormalStream3dATIFunPtr #-}

-- | Usage: @'glNormalStream3dvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glNormalStream3dvATI :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glNormalStream3dvATI = ffienumPtrdoubleIOV glNormalStream3dvATIFunPtr

glNormalStream3dvATIFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glNormalStream3dvATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3dvATI")

{-# NOINLINE glNormalStream3dvATIFunPtr #-}

-- | Usage: @'glNormalStream3fATI' stream nx ny nz@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glNormalStream3fATI :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> m ()
glNormalStream3fATI = ffienumfloatfloatfloatIOV glNormalStream3fATIFunPtr

glNormalStream3fATIFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
glNormalStream3fATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3fATI")

{-# NOINLINE glNormalStream3fATIFunPtr #-}

-- | Usage: @'glNormalStream3fvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glNormalStream3fvATI :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glNormalStream3fvATI = ffienumPtrfloatIOV glNormalStream3fvATIFunPtr

glNormalStream3fvATIFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glNormalStream3fvATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3fvATI")

{-# NOINLINE glNormalStream3fvATIFunPtr #-}

-- | Usage: @'glNormalStream3iATI' stream nx ny nz@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glNormalStream3iATI :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> m ()
glNormalStream3iATI = ffienumintintintIOV glNormalStream3iATIFunPtr

glNormalStream3iATIFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
glNormalStream3iATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3iATI")

{-# NOINLINE glNormalStream3iATIFunPtr #-}

-- | Usage: @'glNormalStream3ivATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glNormalStream3ivATI :: MonadIO m => GLenum -> Ptr GLint -> m ()
glNormalStream3ivATI = ffienumPtrintIOV glNormalStream3ivATIFunPtr

glNormalStream3ivATIFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glNormalStream3ivATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3ivATI")

{-# NOINLINE glNormalStream3ivATIFunPtr #-}

-- | Usage: @'glNormalStream3sATI' stream nx ny nz@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glNormalStream3sATI :: MonadIO m => GLenum -> GLshort -> GLshort -> GLshort -> m ()
glNormalStream3sATI = ffienumshortshortshortIOV glNormalStream3sATIFunPtr

glNormalStream3sATIFunPtr :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
glNormalStream3sATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3sATI")

{-# NOINLINE glNormalStream3sATIFunPtr #-}

-- | Usage: @'glNormalStream3svATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glNormalStream3svATI :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glNormalStream3svATI = ffienumPtrshortIOV glNormalStream3svATIFunPtr

glNormalStream3svATIFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glNormalStream3svATIFunPtr = unsafePerformIO (getProcAddress "glNormalStream3svATI")

{-# NOINLINE glNormalStream3svATIFunPtr #-}

-- | Usage: @'glVertexBlendEnvfATI' pname param@
--
-- The parameter @pname@ is a @VertexStreamATI@.


glVertexBlendEnvfATI :: MonadIO m => GLenum -> GLfloat -> m ()
glVertexBlendEnvfATI = ffienumfloatIOV glVertexBlendEnvfATIFunPtr

glVertexBlendEnvfATIFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glVertexBlendEnvfATIFunPtr = unsafePerformIO (getProcAddress "glVertexBlendEnvfATI")

{-# NOINLINE glVertexBlendEnvfATIFunPtr #-}

-- | Usage: @'glVertexBlendEnviATI' pname param@
--
-- The parameter @pname@ is a @VertexStreamATI@.


glVertexBlendEnviATI :: MonadIO m => GLenum -> GLint -> m ()
glVertexBlendEnviATI = ffienumintIOV glVertexBlendEnviATIFunPtr

glVertexBlendEnviATIFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glVertexBlendEnviATIFunPtr = unsafePerformIO (getProcAddress "glVertexBlendEnviATI")

{-# NOINLINE glVertexBlendEnviATIFunPtr #-}

-- | Usage: @'glVertexStream1dATI' stream x@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream1dATI :: MonadIO m => GLenum -> GLdouble -> m ()
glVertexStream1dATI = ffienumdoubleIOV glVertexStream1dATIFunPtr

glVertexStream1dATIFunPtr :: FunPtr (GLenum -> GLdouble -> IO ())
glVertexStream1dATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1dATI")

{-# NOINLINE glVertexStream1dATIFunPtr #-}

-- | Usage: @'glVertexStream1dvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @1@.


glVertexStream1dvATI :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glVertexStream1dvATI = ffienumPtrdoubleIOV glVertexStream1dvATIFunPtr

glVertexStream1dvATIFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glVertexStream1dvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1dvATI")

{-# NOINLINE glVertexStream1dvATIFunPtr #-}

-- | Usage: @'glVertexStream1fATI' stream x@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream1fATI :: MonadIO m => GLenum -> GLfloat -> m ()
glVertexStream1fATI = ffienumfloatIOV glVertexStream1fATIFunPtr

glVertexStream1fATIFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glVertexStream1fATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1fATI")

{-# NOINLINE glVertexStream1fATIFunPtr #-}

-- | Usage: @'glVertexStream1fvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @1@.


glVertexStream1fvATI :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glVertexStream1fvATI = ffienumPtrfloatIOV glVertexStream1fvATIFunPtr

glVertexStream1fvATIFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glVertexStream1fvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1fvATI")

{-# NOINLINE glVertexStream1fvATIFunPtr #-}

-- | Usage: @'glVertexStream1iATI' stream x@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream1iATI :: MonadIO m => GLenum -> GLint -> m ()
glVertexStream1iATI = ffienumintIOV glVertexStream1iATIFunPtr

glVertexStream1iATIFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glVertexStream1iATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1iATI")

{-# NOINLINE glVertexStream1iATIFunPtr #-}

-- | Usage: @'glVertexStream1ivATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @1@.


glVertexStream1ivATI :: MonadIO m => GLenum -> Ptr GLint -> m ()
glVertexStream1ivATI = ffienumPtrintIOV glVertexStream1ivATIFunPtr

glVertexStream1ivATIFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glVertexStream1ivATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1ivATI")

{-# NOINLINE glVertexStream1ivATIFunPtr #-}

-- | Usage: @'glVertexStream1sATI' stream x@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream1sATI :: MonadIO m => GLenum -> GLshort -> m ()
glVertexStream1sATI = ffienumshortIOV glVertexStream1sATIFunPtr

glVertexStream1sATIFunPtr :: FunPtr (GLenum -> GLshort -> IO ())
glVertexStream1sATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1sATI")

{-# NOINLINE glVertexStream1sATIFunPtr #-}

-- | Usage: @'glVertexStream1svATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @1@.


glVertexStream1svATI :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glVertexStream1svATI = ffienumPtrshortIOV glVertexStream1svATIFunPtr

glVertexStream1svATIFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glVertexStream1svATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream1svATI")

{-# NOINLINE glVertexStream1svATIFunPtr #-}

-- | Usage: @'glVertexStream2dATI' stream x y@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream2dATI :: MonadIO m => GLenum -> GLdouble -> GLdouble -> m ()
glVertexStream2dATI = ffienumdoubledoubleIOV glVertexStream2dATIFunPtr

glVertexStream2dATIFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
glVertexStream2dATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2dATI")

{-# NOINLINE glVertexStream2dATIFunPtr #-}

-- | Usage: @'glVertexStream2dvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @2@.


glVertexStream2dvATI :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glVertexStream2dvATI = ffienumPtrdoubleIOV glVertexStream2dvATIFunPtr

glVertexStream2dvATIFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glVertexStream2dvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2dvATI")

{-# NOINLINE glVertexStream2dvATIFunPtr #-}

-- | Usage: @'glVertexStream2fATI' stream x y@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream2fATI :: MonadIO m => GLenum -> GLfloat -> GLfloat -> m ()
glVertexStream2fATI = ffienumfloatfloatIOV glVertexStream2fATIFunPtr

glVertexStream2fATIFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
glVertexStream2fATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2fATI")

{-# NOINLINE glVertexStream2fATIFunPtr #-}

-- | Usage: @'glVertexStream2fvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @2@.


glVertexStream2fvATI :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glVertexStream2fvATI = ffienumPtrfloatIOV glVertexStream2fvATIFunPtr

glVertexStream2fvATIFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glVertexStream2fvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2fvATI")

{-# NOINLINE glVertexStream2fvATIFunPtr #-}

-- | Usage: @'glVertexStream2iATI' stream x y@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream2iATI :: MonadIO m => GLenum -> GLint -> GLint -> m ()
glVertexStream2iATI = ffienumintintIOV glVertexStream2iATIFunPtr

glVertexStream2iATIFunPtr :: FunPtr (GLenum -> GLint -> GLint -> IO ())
glVertexStream2iATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2iATI")

{-# NOINLINE glVertexStream2iATIFunPtr #-}

-- | Usage: @'glVertexStream2ivATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @2@.


glVertexStream2ivATI :: MonadIO m => GLenum -> Ptr GLint -> m ()
glVertexStream2ivATI = ffienumPtrintIOV glVertexStream2ivATIFunPtr

glVertexStream2ivATIFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glVertexStream2ivATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2ivATI")

{-# NOINLINE glVertexStream2ivATIFunPtr #-}

-- | Usage: @'glVertexStream2sATI' stream x y@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream2sATI :: MonadIO m => GLenum -> GLshort -> GLshort -> m ()
glVertexStream2sATI = ffienumshortshortIOV glVertexStream2sATIFunPtr

glVertexStream2sATIFunPtr :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
glVertexStream2sATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2sATI")

{-# NOINLINE glVertexStream2sATIFunPtr #-}

-- | Usage: @'glVertexStream2svATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @2@.


glVertexStream2svATI :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glVertexStream2svATI = ffienumPtrshortIOV glVertexStream2svATIFunPtr

glVertexStream2svATIFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glVertexStream2svATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream2svATI")

{-# NOINLINE glVertexStream2svATIFunPtr #-}

-- | Usage: @'glVertexStream3dATI' stream x y z@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream3dATI :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> m ()
glVertexStream3dATI = ffienumdoubledoubledoubleIOV glVertexStream3dATIFunPtr

glVertexStream3dATIFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
glVertexStream3dATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3dATI")

{-# NOINLINE glVertexStream3dATIFunPtr #-}

-- | Usage: @'glVertexStream3dvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glVertexStream3dvATI :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glVertexStream3dvATI = ffienumPtrdoubleIOV glVertexStream3dvATIFunPtr

glVertexStream3dvATIFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glVertexStream3dvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3dvATI")

{-# NOINLINE glVertexStream3dvATIFunPtr #-}

-- | Usage: @'glVertexStream3fATI' stream x y z@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream3fATI :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> m ()
glVertexStream3fATI = ffienumfloatfloatfloatIOV glVertexStream3fATIFunPtr

glVertexStream3fATIFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
glVertexStream3fATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3fATI")

{-# NOINLINE glVertexStream3fATIFunPtr #-}

-- | Usage: @'glVertexStream3fvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glVertexStream3fvATI :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glVertexStream3fvATI = ffienumPtrfloatIOV glVertexStream3fvATIFunPtr

glVertexStream3fvATIFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glVertexStream3fvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3fvATI")

{-# NOINLINE glVertexStream3fvATIFunPtr #-}

-- | Usage: @'glVertexStream3iATI' stream x y z@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream3iATI :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> m ()
glVertexStream3iATI = ffienumintintintIOV glVertexStream3iATIFunPtr

glVertexStream3iATIFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
glVertexStream3iATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3iATI")

{-# NOINLINE glVertexStream3iATIFunPtr #-}

-- | Usage: @'glVertexStream3ivATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glVertexStream3ivATI :: MonadIO m => GLenum -> Ptr GLint -> m ()
glVertexStream3ivATI = ffienumPtrintIOV glVertexStream3ivATIFunPtr

glVertexStream3ivATIFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glVertexStream3ivATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3ivATI")

{-# NOINLINE glVertexStream3ivATIFunPtr #-}

-- | Usage: @'glVertexStream3sATI' stream x y z@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream3sATI :: MonadIO m => GLenum -> GLshort -> GLshort -> GLshort -> m ()
glVertexStream3sATI = ffienumshortshortshortIOV glVertexStream3sATIFunPtr

glVertexStream3sATIFunPtr :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
glVertexStream3sATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3sATI")

{-# NOINLINE glVertexStream3sATIFunPtr #-}

-- | Usage: @'glVertexStream3svATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @3@.


glVertexStream3svATI :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glVertexStream3svATI = ffienumPtrshortIOV glVertexStream3svATIFunPtr

glVertexStream3svATIFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glVertexStream3svATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream3svATI")

{-# NOINLINE glVertexStream3svATIFunPtr #-}

-- | Usage: @'glVertexStream4dATI' stream x y z w@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream4dATI :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glVertexStream4dATI = ffienumdoubledoubledoubledoubleIOV glVertexStream4dATIFunPtr

glVertexStream4dATIFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glVertexStream4dATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4dATI")

{-# NOINLINE glVertexStream4dATIFunPtr #-}

-- | Usage: @'glVertexStream4dvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @4@.


glVertexStream4dvATI :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glVertexStream4dvATI = ffienumPtrdoubleIOV glVertexStream4dvATIFunPtr

glVertexStream4dvATIFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glVertexStream4dvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4dvATI")

{-# NOINLINE glVertexStream4dvATIFunPtr #-}

-- | Usage: @'glVertexStream4fATI' stream x y z w@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream4fATI :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glVertexStream4fATI = ffienumfloatfloatfloatfloatIOV glVertexStream4fATIFunPtr

glVertexStream4fATIFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glVertexStream4fATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4fATI")

{-# NOINLINE glVertexStream4fATIFunPtr #-}

-- | Usage: @'glVertexStream4fvATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @4@.


glVertexStream4fvATI :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glVertexStream4fvATI = ffienumPtrfloatIOV glVertexStream4fvATIFunPtr

glVertexStream4fvATIFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glVertexStream4fvATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4fvATI")

{-# NOINLINE glVertexStream4fvATIFunPtr #-}

-- | Usage: @'glVertexStream4iATI' stream x y z w@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream4iATI :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> m ()
glVertexStream4iATI = ffienumintintintintIOV glVertexStream4iATIFunPtr

glVertexStream4iATIFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
glVertexStream4iATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4iATI")

{-# NOINLINE glVertexStream4iATIFunPtr #-}

-- | Usage: @'glVertexStream4ivATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @4@.


glVertexStream4ivATI :: MonadIO m => GLenum -> Ptr GLint -> m ()
glVertexStream4ivATI = ffienumPtrintIOV glVertexStream4ivATIFunPtr

glVertexStream4ivATIFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glVertexStream4ivATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4ivATI")

{-# NOINLINE glVertexStream4ivATIFunPtr #-}

-- | Usage: @'glVertexStream4sATI' stream x y z w@
--
-- The parameter @stream@ is a @VertexStreamATI@.


glVertexStream4sATI :: MonadIO m => GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
glVertexStream4sATI = ffienumshortshortshortshortIOV glVertexStream4sATIFunPtr

glVertexStream4sATIFunPtr :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
glVertexStream4sATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4sATI")

{-# NOINLINE glVertexStream4sATIFunPtr #-}

-- | Usage: @'glVertexStream4svATI' stream coords@
--
-- The parameter @stream@ is a @VertexStreamATI@.
--
-- The length of @coords@ should be @4@.


glVertexStream4svATI :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glVertexStream4svATI = ffienumPtrshortIOV glVertexStream4svATIFunPtr

glVertexStream4svATIFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glVertexStream4svATIFunPtr = unsafePerformIO (getProcAddress "glVertexStream4svATI")

{-# NOINLINE glVertexStream4svATIFunPtr #-}

pattern GL_MAX_VERTEX_STREAMS_ATI  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_STREAMS_ATI = 0x876B

pattern GL_VERTEX_SOURCE_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_SOURCE_ATI = 0x8774

pattern GL_VERTEX_STREAM0_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM0_ATI = 0x876C

pattern GL_VERTEX_STREAM1_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM1_ATI = 0x876D

pattern GL_VERTEX_STREAM2_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM2_ATI = 0x876E

pattern GL_VERTEX_STREAM3_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM3_ATI = 0x876F

pattern GL_VERTEX_STREAM4_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM4_ATI = 0x8770

pattern GL_VERTEX_STREAM5_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM5_ATI = 0x8771

pattern GL_VERTEX_STREAM6_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM6_ATI = 0x8772

pattern GL_VERTEX_STREAM7_ATI  :: (Eq a, Num a) => a

pattern GL_VERTEX_STREAM7_ATI = 0x8773